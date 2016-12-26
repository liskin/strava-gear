{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module StravaGear.Sync
    ( sync
    )
  where

import Protolude hiding (from, isNothing, isPrefixOf, on)

import Control.Arrow ((&&&))
import qualified Data.Map as Map ((!), fromList)
import qualified Data.Set as Set (empty, insert, member)
import System.IO.Unsafe (unsafePerformIO)

import Data.Text (isPrefixOf, words)
import Data.Time (UTCTime, getCurrentTime)
import Database.Esqueleto
    ( InnerJoin(InnerJoin)
    , Value(Value)
    , (^.), (==.), (<=.), (>=.), (||.)
    , from
    , groupBy
    , in_
    , isNothing
    , just
    , max_
    , on
    , select
    , valList
    , where_
    )
import Database.Persist
    ( Entity(Entity)
    , SelectOpt(Desc)
    , (<-.)
    , entityKey
    , entityVal
    , selectFirst
    , selectKeysList
    , selectList
    )
import Database.Persist.Sql (SqlPersistM, runMigration)
import Database.Persist.Sqlite (runSqlite)
import qualified Strive as S

import StravaGear.Config
import StravaGear.Database.Schema
import StravaGear.Database.Utils


sync :: Bool -> S.Client -> IO Text
sync forceFetch client = do
    Right athlete <- S.getCurrentAthlete client
    let athleteId = S.id `S.get` athlete :: Integer
        athleteBikes = S.bikes `S.get` athlete
        sqliteFileName = "athlete_" ++ show athleteId ++ ".sqlite"
        confFileName = "athlete_" ++ show athleteId ++ ".conf"
    conf <- readFile confFileName
    runSqlite (toS sqliteFileName) $ do
        runMigration migrateAll
        bUpsert <- syncBikes athleteBikes
        aUpsert <- syncActivities forceFetch client
        (cUpsert, rUpsert, lUpsert, hUpsert) <- syncConfig conf
        let changed = (bUpsert, aUpsert, cUpsert, rUpsert, lUpsert, hUpsert)
        let newActs = activitiesToRefresh changed
        syncHashTags newActs
        syncActivitiesComponents newActs
    return $ toS sqliteFileName

type WhatChanged =
    ( [UpsertResult Bike]
    , [UpsertResult Activity]
    , [UpsertResult Component]
    , [UpsertResult ComponentRole]
    , [UpsertResult LongtermBikeComponent]
    , [UpsertResult HashTagBikeComponent] )

activitiesToRefresh :: WhatChanged -> Maybe [Key Activity]
activitiesToRefresh (bUpsert, aUpsert, cUpsert, rUpsert, lUpsert, hUpsert) =
    case onlyNoop bUpsert && confNoop of
        True -> case changedEntities aUpsert of
            ch | length ch < 100 -> Just ch
               | otherwise -> Nothing -- damn sqlite :-/
        False -> Nothing
    where
        isNoop (UpsertNoop _) = True
        isNoop _ = False
        onlyNoop = null . filter (not . isNoop)
        confNoop = onlyNoop cUpsert && onlyNoop rUpsert
                && onlyNoop lUpsert && onlyNoop hUpsert

syncBikes :: [S.GearSummary] -> SqlPersistM [UpsertResult Bike]
syncBikes bikes = syncEntitiesDel $ map bike bikes
    where bike b = Bike (S.name `S.get` b) (S.id `S.get` b)

syncActivities :: Bool -> S.Client -> SqlPersistM [UpsertResult Activity]
syncActivities forceFetch client = do
    fetchRes <- fetchActivitiesFast forceFetch client
    case fetchRes of
        Nothing ->
            fmap (map $ \k -> UpsertNoop (Entity k undefined)) $
                selectKeysList [] []
        Just acts ->
            syncEntitiesDel $ map act acts
    where
        act a = Activity
            { activityStravaId = fromIntegral $ S.id `S.get` a
            , activityName = S.name `S.get` a
            , activityStartTime = S.startDate `S.get` a
            , activityMovingTime = fromIntegral $ S.movingTime `S.get` a
            , activityDistance = S.distance `S.get` a
            , activityGearId = S.gearId `S.get` a
            }

fetchActivities :: S.Client -> SqlPersistM [S.ActivitySummary]
fetchActivities client = do
    now <- liftIO $ getCurrentTime
    concat `fmap` go now
    where
        go t = do
            liftIO $ putStrLn $ "fetching activities before " ++ show t
            acts <- fetchActivitiesBefore client t
            case lastMay acts of
                Nothing -> pure [acts]
                Just lastAct ->
                    fmap (acts :) $ go (S.startDate `S.get` lastAct)

-- | Check if we have the newest activity and skip sync if we do, unless
-- forced to refresh.
fetchActivitiesFast :: Bool -> S.Client -> SqlPersistM (Maybe [S.ActivitySummary])
fetchActivitiesFast True client =
    Just `fmap` fetchActivities client
fetchActivitiesFast False client = do
    newest <- selectFirst [] [Desc ActivityStartTime]
    now <- liftIO $ getCurrentTime
    acts <- fetchActivitiesBefore client now
    case (newest, acts) of
        (Just (Entity _ a1), a2:_)
            | fromIntegral (S.id `S.get` a2) == activityStravaId a1 ->
                return Nothing
        _ ->
            fetchActivitiesFast True client

fetchActivitiesBefore :: S.Client -> UTCTime -> SqlPersistM [S.ActivitySummary]
fetchActivitiesBefore client t =
    fromRightM $ liftIO $ S.getCurrentActivities client $
        S.with [ S.before `S.set` Just t ]
  where
    fromRightM :: (Monad m, Show a) => m (Either a b) -> m b
    fromRightM = fmap (either (panic . show) identity)


syncHashTags :: Maybe [Key Activity] -> SqlPersistM ()
syncHashTags new = do
    let filt = case new of
            Nothing -> []
            Just n -> [ActivityId <-. n]
    acts <- selectList filt []
    let actTags = [ (entityKey e, actHashTags $ entityVal e) | e <- acts ]
        allTags = distinctUsing identity $ concatMap snd actTags
    tagsRes <- syncEntities allTags
    let tagMap = Map.fromList [ (entityVal e, entityKey e)
                              | e <- keptEntities tagsRes ]
    old <- mapM activityActivityHashTags new
    wipeInsertMany old
        [ ActivityHashTag k (tagMap Map.! t) | (k, ts) <- actTags, t <- ts ]

actHashTags :: Activity -> [HashTag]
actHashTags Activity{activityName = name} =
    [ HashTag w | w <- words name, "#" `isPrefixOf` w ]

activityActivityHashTags :: [Key Activity] -> SqlPersistM [Key ActivityHashTag]
activityActivityHashTags as =
    fmap (map (\(Value v) -> v)) $ select $ from $ \ah -> do
        where_ $ ah ^. ActivityHashTagActivity `in_` valList as
        return $ ah ^. ActivityHashTagId

syncActivitiesComponents :: Maybe [Key Activity] -> SqlPersistM ()
syncActivitiesComponents new = do
    l <- activitiesLongtermComponents new
    h <- activitiesHashtagComponents new
    let dedup = distinctUsing $ activityComponentActivity &&& activityComponentRole
    old <- mapM activityActivityComponents new
    wipeInsertMany old $ dedup $ h ++ l

activityActivityComponents :: [Key Activity] -> SqlPersistM [Key ActivityComponent]
activityActivityComponents as =
    fmap (map (\(Value v) -> v)) $ select $ from $ \ac -> do
        where_ $ ac ^. ActivityComponentActivity `in_` valList as
        return $ ac ^. ActivityComponentId

activitiesLongtermComponents :: Maybe [Key Activity] -> SqlPersistM [ActivityComponent]
activitiesLongtermComponents new = do
    let f (_, Value k, Value c, Value r) = ActivityComponent k c r
    fmap (map f) $ select $ from $ \(a `InnerJoin` b `InnerJoin` lt) -> do
        on $ a ^. ActivityGearId ==. just (b ^. BikeStravaId)
        on $ b ^. BikeId ==. lt ^. LongtermBikeComponentBike
        mapM_ (\n -> where_ $ a ^. ActivityId `in_` valList n) new
        where_ $ lt ^. LongtermBikeComponentStartTime <=. a ^. ActivityStartTime
        where_ $ lt ^. LongtermBikeComponentEndTime >=. just (a ^. ActivityStartTime)
            ||. isNothing (lt ^. LongtermBikeComponentEndTime)
        groupBy (a ^. ActivityId, lt ^. LongtermBikeComponentRole)
        return ( castToPersistValue (max_ $ lt ^. LongtermBikeComponentStartTime)
               , a ^. ActivityId
               , lt ^. LongtermBikeComponentComponent
               , lt ^. LongtermBikeComponentRole)

activitiesHashtagComponents :: Maybe [Key Activity] -> SqlPersistM [ActivityComponent]
activitiesHashtagComponents new = do
    let f (_, Value k, Value c, Value r) = ActivityComponent k c r
    fmap (map f) $ select $ from $ \(a `InnerJoin` ah `InnerJoin` h) -> do
        on $ a ^. ActivityId ==. ah ^. ActivityHashTagActivity
        on $ ah ^. ActivityHashTagTag ==. h ^. HashTagBikeComponentTag
        mapM_ (\n -> where_ $ a ^. ActivityId `in_` valList n) new
        where_ $ h ^. HashTagBikeComponentStartTime <=. a ^. ActivityStartTime
        where_ $ h ^. HashTagBikeComponentEndTime >=. just (a ^. ActivityStartTime)
            ||. isNothing (h ^. HashTagBikeComponentEndTime)
        groupBy (a ^. ActivityId, h ^. HashTagBikeComponentRole)
        return ( castToPersistValue (max_ $ h ^. HashTagBikeComponentStartTime)
               , a ^. ActivityId
               , h ^. HashTagBikeComponentComponent
               , h ^. HashTagBikeComponentRole)

syncConfig :: Text -> SqlPersistM ( [UpsertResult Component]
                                  , [UpsertResult ComponentRole]
                                  , [UpsertResult LongtermBikeComponent]
                                  , [UpsertResult HashTagBikeComponent] )
syncConfig conf = do
    let cs = parseConf conf
    components <- syncEntitiesDel
        [ Component c n dur dist | ConfComponent c n dur dist <- cs ]
    roles <- syncEntitiesDel
        [ ComponentRole n | ConfRole n <- cs ]
    bikes <- selectList [] []
    let allTags = distinctUsing identity
            [ HashTag t | ConfHashTag t _ _ _ _ <- cs ]
    tags <- syncEntities allTags
    let componentMap = Map.fromList
            [ (componentUniqueId v, k) | Entity k v <- keptEntities components ]
        roleMap = Map.fromList
            [ (componentRoleName v, k) | Entity k v <- keptEntities roles ]
        bikeMap = Map.fromList
            [ (bikeStravaId v, k) | Entity k v <- bikes ]
        tagMap = Map.fromList
            [ (hashTagName t, k) | Entity k t <- keptEntities tags ]
    longterms <- syncEntitiesDel
        [ LongtermBikeComponent (componentMap ! c)
                                (bikeMap ! b) (roleMap ! r) s e
        | ConfLongterm c b r s e <- cs ]
    hashtags <- syncEntitiesDel
        [ HashTagBikeComponent (tagMap ! t) (componentMap ! c) (roleMap ! r) s e
        | ConfHashTag t c r s e <- cs ]
    return (components, roles, longterms, hashtags)
  where
    (!) :: (Ord k, Show k, Show a) => Map k a -> k -> a
    m ! v = unsafePerformIO $ try (evaluate (m Map.! v)) >>= \case
        Right x -> return x
        Left e ->
            panic . toS $ show m ++ " ! " ++ show v ++ ": " ++ show (e :: SomeException)

distinctUsing :: (Ord b) => (a -> b) -> [a] -> [a]
distinctUsing f = go Set.empty
    where
        go _ [] = []
        go seen (x:xs) =
            let x' = f x
            in if x' `Set.member` seen
                then go seen xs
                else x : go (x' `Set.insert` seen) xs
