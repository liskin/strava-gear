{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module StravaGear.Sync
    ( SyncStravaRes(..)
    , syncConfig
    , syncStrava
    )
  where

import Protolude hiding (all, from, isNothing, isPrefixOf, on)

import Control.Arrow ((&&&))
import qualified Data.Map as Map ((!), fromList)
import qualified Data.Set as Set (empty, insert, member)
import System.IO.Unsafe (unsafePerformIO)

import Data.Aeson (ToJSON)
import Data.Char (isDigit)
import Data.Text (all, splitOn, stripPrefix, words)
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
    , selectList
    )
import Database.Persist.Sql (SqlPersistM)
import qualified Strive as S

import StravaGear.Config (Conf(..))
import StravaGear.Database.Schema
import StravaGear.Database.Utils
import StravaGear.Types
    ( BikeText(BikeText)
    , HashTagText(HashTagText)
    )


data SyncStravaRes = SyncStravaRes
    { updatedBikes :: !Int
    , updatedActivities :: !Int
    }
  deriving (Generic, ToJSON)

syncStrava :: Bool -> S.Client -> SqlPersistM SyncStravaRes
syncStrava forceFetch client = do
    bUpsert <- syncBikes client
    aUpsert <- syncActivities forceFetch client
    let newActs = activitiesToRefresh (bUpsert, aUpsert)
    syncHashTags newActs
    syncActivitiesComponents newActs
    pure $ SyncStravaRes
        { updatedBikes = length $ changedEntities bUpsert
        , updatedActivities = length $ changedEntities aUpsert
        }

activitiesToRefresh
    :: ([UpsertResult Bike], [UpsertResult Activity])
    -> Maybe [Key Activity]
activitiesToRefresh (bUpsert, aUpsert) =
    case (changedEntities bUpsert, changedEntities aUpsert) of
        ([], ch) | length ch < 100 {- damn sqlite :-/ -} -> Just ch
        _ -> Nothing

syncBikes :: S.Client -> SqlPersistM [UpsertResult Bike]
syncBikes client = do
    Right athlete <- liftIO $ S.getCurrentAthlete client
    syncEntitiesDel $ map bike $ S.bikes `S.get` athlete
  where
    bike b = Bike (S.name `S.get` b) (BikeText $ S.id `S.get` b)

syncActivities :: Bool -> S.Client -> SqlPersistM [UpsertResult Activity]
syncActivities forceFetch client = do
    fetchRes <- fetchActivitiesFast forceFetch client
    case fetchRes of
        Nothing -> pure mempty
        Just acts -> syncEntitiesDel $ map act acts
    where
        act a = Activity
            { activityStravaId = fromIntegral $ S.id `S.get` a
            , activityName = S.name `S.get` a
            , activityStartTime = S.startDate `S.get` a
            , activityMovingTime = fromIntegral $ S.movingTime `S.get` a
            , activityDistance = S.distance `S.get` a
            , activityGearId = BikeText <$> S.gearId `S.get` a
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
    [ HashTag (HashTagText w)
    | w <- concatMap words . splitOn ", " $ name
    , Just w' <- pure $ stripPrefix "#" w
    , not (all isDigit w')
    ]

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

syncConfig :: [Conf] -> SqlPersistM ()
syncConfig cs = do
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
        | ConfLongterm b r c s e <- cs ]
    hashtags <- syncEntitiesDel
        [ HashTagBikeComponent (tagMap ! t) (componentMap ! c) (roleMap ! r) s e
        | ConfHashTag t r c s e <- cs ]

    let noop = null (changedEntities components)
            && null (changedEntities roles)
            && null (changedEntities longterms)
            && null (changedEntities hashtags)
    unless noop $ do
        syncHashTags Nothing
        syncActivitiesComponents Nothing
  where
    (!) :: (Ord k, Show k, Show a) => Map k a -> k -> a
    m ! v = unsafePerformIO $ try (evaluate (m Map.! v)) >>= \case
        Right x -> return x
        Left e ->
            panic $ show m <> " ! " <> show v <> ": " <> show (e :: SomeException)

distinctUsing :: (Ord b) => (a -> b) -> [a] -> [a]
distinctUsing f = go Set.empty
    where
        go _ [] = []
        go seen (x:xs) =
            let x' = f x
            in if x' `Set.member` seen
                then go seen xs
                else x : go (x' `Set.insert` seen) xs
