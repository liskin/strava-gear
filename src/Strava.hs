{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Strava where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.List ((\\))
import Data.List.Split (chunksOf)
import Data.Time
import Database.Esqueleto
import Database.Esqueleto.Internal.Sql (veryUnsafeCoerceSqlExprValue)
import Database.Persist (selectKeysList, selectList, selectFirst, SelectOpt(..), (<-.))
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Strive as S
import qualified Text.Tabular as Tab
import qualified Text.Tabular.AsciiArt as Tab

import Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
    Bike
        name T.Text
        stravaId T.Text
        StravaBikeId stravaId
        deriving Eq Ord Show

    Component
        uniqueId T.Text
        UniqueId uniqueId
        name T.Text
        initialSeconds Int -- seconds
        initialMeters Double -- meters
        deriving Eq Ord Show

    ComponentRole
        name T.Text
        UniqueName name
        deriving Eq Ord Show

    LongtermBikeComponent
        component ComponentId
        bike BikeId
        role ComponentRoleId
        startTime UTCTime
        endTime UTCTime Maybe
        UniqueLongtermBikeComponent component bike role startTime
        deriving Eq Ord Show

    Activity
        stravaId Int
        StravaActivityId stravaId
        name T.Text
        startTime UTCTime
        movingTime Int -- seconds
        distance Double -- meters
        gearId T.Text Maybe
        deriving Eq Ord Show

    ActivityComponent
        activity ActivityId
        component ComponentId
        role ComponentRoleId
        deriving Eq Ord Show

    HashTagBikeComponent
        tag HashTagId
        component ComponentId
        role ComponentRoleId
        startTime UTCTime
        endTime UTCTime Maybe
        UniqueHashTagBikeComponent tag component role startTime
        deriving Eq Ord Show

    HashTag
        name T.Text
        UniqueHashTag name
        deriving Eq Ord Show

    ActivityHashTag
        activity ActivityId
        tag HashTagId
        UniqueActivityHashTag activity tag
        deriving Eq Ord Show
|]

deriving instance Eq (Unique Bike)
deriving instance Eq (Unique Component)
deriving instance Eq (Unique ComponentRole)
deriving instance Eq (Unique Activity)
deriving instance Eq (Unique LongtermBikeComponent)
deriving instance Eq (Unique HashTagBikeComponent)
deriving instance Eq (Unique HashTag)


-- Sync --

testClient :: IO S.Client
testClient = S.buildClient (Just $ T.pack token)

sync :: Bool -> S.Client -> IO T.Text
sync forceFetch client = do
    Right athlete <- S.getCurrentAthlete client
    let athleteId = S.id `S.get` athlete :: Integer
        athleteBikes = S.bikes `S.get` athlete
        sqliteFileName = "athlete_" ++ show athleteId ++ ".sqlite"
        confFileName = "athlete_" ++ show athleteId ++ ".conf"
    conf <- T.readFile confFileName
    runSqlite (T.pack sqliteFileName) $ do
        runMigration migrateAll
        bUpsert <- syncBikes athleteBikes
        aUpsert <- syncActivities forceFetch client
        (cUpsert, rUpsert, lUpsert, hUpsert) <- syncConfig conf
        let changed = (bUpsert, aUpsert, cUpsert, rUpsert, lUpsert, hUpsert)
            newActs = activitiesToRefresh changed
        syncHashTags newActs
        syncActivitiesComponents newActs
    return $ T.pack sqliteFileName

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
            if null acts
                then return [acts]
                else fmap (acts :) $ go (S.startDate `S.get` last acts)

-- | Check if we have the newest activity and skip sync if we do, unless
-- forced to refresh.
fetchActivitiesFast :: Bool -> S.Client -> SqlPersistM (Maybe [S.ActivitySummary])
fetchActivitiesFast True client =
    Just `fmap` fetchActivities client
fetchActivitiesFast False client = do
    first <- selectFirst [] [Desc ActivityStartTime]
    now <- liftIO $ getCurrentTime
    acts <- fetchActivitiesBefore client now
    case (first, acts) of
        (Just (Entity _ a1), a2:_)
            | fromIntegral (S.id `S.get` a2) == activityStravaId a1 ->
                return Nothing
        _ ->
            fetchActivitiesFast True client

fetchActivitiesBefore :: S.Client -> UTCTime -> SqlPersistM [S.ActivitySummary]
fetchActivitiesBefore client t = do
    fromRightM $ liftIO $ S.getCurrentActivities client $
        S.with [ S.before `S.set` Just t ]

syncHashTags :: Maybe [Key Activity] -> SqlPersistM ()
syncHashTags new = do
    let filt = case new of
            Nothing -> []
            Just n -> [ActivityId <-. n]
    acts <- selectList filt []
    let actTags = [ (entityKey e, actHashTags $ entityVal e) | e <- acts ]
        allTags = distinctUsing id $ concatMap snd actTags
    tagsRes <- syncEntities allTags
    let tagMap = Map.fromList [ (entityVal e, entityKey e)
                              | e <- keptEntities tagsRes ]
    old <- mapM activityActivityHashTags new
    wipeInsertMany old
        [ ActivityHashTag k (tagMap Map.! t) | (k, ts) <- actTags, t <- ts ]

actHashTags :: Activity -> [HashTag]
actHashTags Activity{activityName = name} =
    [ HashTag w | w <- T.words name, "#" `T.isPrefixOf` w ]

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


-- Report --

report :: T.Text -> IO ()
report fileName = do
    runSqlite fileName $ do
        tab <- componentReport
        liftIO $ putStr $ Tab.render id id id tab
        return ()

componentReport :: SqlPersistM (Tab.Table String String String)
componentReport = do
    res <- select $
        from $ \(c `InnerJoin` ac `InnerJoin` a `InnerJoin` r) -> do
            on (c ^. ComponentId ==. ac ^. ActivityComponentComponent)
            on (ac ^. ActivityComponentActivity ==. a ^. ActivityId)
            on (ac ^. ActivityComponentRole ==. r ^. ComponentRoleId)
            groupBy (c ^. ComponentId)
            orderBy
                [ asc $ r ^. ComponentRoleName
                , asc $ c ^. ComponentUniqueId ]
            let iniTime = just $ c ^. ComponentInitialSeconds
                iniDist = just $ c ^. ComponentInitialMeters
                sumMovingTime = sum_ $ a ^. ActivityMovingTime
                sumDist = sum_ $ a ^. ActivityDistance
                firstUsage = min_ $ a ^. ActivityStartTime
                lastUsage = max_ $ a ^. ActivityStartTime
            return
                ( r, c
                , iniTime +. sumMovingTime
                , iniDist +. sumDist
                , firstUsage, lastUsage )
    let ids = [ T.unpack $ componentRoleName r
              | (Entity _ r, _, _, _, _, _) <- res ]
        rh = Tab.Group Tab.NoLine (map Tab.Header ids)
        ch = Tab.Group Tab.DoubleLine
            [ Tab.Group Tab.SingleLine [Tab.Header "id", Tab.Header "name"]
            , Tab.Group Tab.SingleLine [Tab.Header "first", Tab.Header "last"]
            , Tab.Group Tab.SingleLine [Tab.Header "time", Tab.Header "distance"]
            ]
        timeFormat = formatTime defaultTimeLocale "%F"
        tab = [ [ T.unpack $ componentUniqueId c, T.unpack $ componentName c
                , timeFormat firstUsage, timeFormat lastUsage, niceTime, niceDist ]
              | ( _, Entity _ c
                , Value (Just time), Value (Just dist)
                , Value (Just firstUsage), Value (Just lastUsage)
                ) <- res
              , let niceTime = printf "%.1f" ((fromIntegral time :: Double) / 3600) ++ " hours"
              , let niceDist = printf "%.0f" ((dist :: Double) / 1000) ++ " km" ]
    return $ Tab.Table rh ch tab


-- Text config (to be replaced by REST) --

syncConfig :: T.Text -> SqlPersistM ( [UpsertResult Component]
                                    , [UpsertResult ComponentRole]
                                    , [UpsertResult LongtermBikeComponent]
                                    , [UpsertResult HashTagBikeComponent] )
syncConfig conf = do
    let ls = T.lines conf
        cs = concatMap parseConf ls
    components <- syncEntitiesDel
        [ Component c n dur dist | ConfComponent c n dur dist <- cs ]
    roles <- syncEntitiesDel
        [ ComponentRole n | ConfRole n <- cs ]
    bikes <- selectList [] []
    let allTags = distinctUsing id [ HashTag t | ConfHashTag t _ _ _ _ <- cs ]
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

data Conf
    = ConfComponent T.Text T.Text Int Double
    | ConfRole T.Text
    | ConfLongterm T.Text T.Text T.Text UTCTime (Maybe UTCTime)
    | ConfHashTag T.Text T.Text T.Text UTCTime (Maybe UTCTime)

parseConf :: T.Text -> [Conf]
parseConf l = case T.words l of
    comment:_ | "#" `T.isPrefixOf` comment -> []
    [] -> []
    ["component", code, name, iniDur, iniDist] ->
        [ConfComponent code name (parseDuration iniDur) (parseDist iniDist)]
    ["role", name] ->
        [ConfRole name]
    ["longterm", component, bike, role, start, end] ->
        [ConfLongterm component bike role (parseUTCTime start)
                                          (Just $ parseUTCTime end)]
    ["longterm", component, bike, role, start] ->
        [ConfLongterm component bike role (parseUTCTime start) Nothing]
    ["hashtag", tag, component, role, start, end] ->
        [ConfHashTag tag component role (parseUTCTime start)
                                        (Just $ parseUTCTime end)]
    ["hashtag", tag, component, role, start] ->
        [ConfHashTag tag component role (parseUTCTime start) Nothing]
    _ ->
        error $ "malformed config line: " ++ T.unpack l

parseDuration :: T.Text -> Int
parseDuration (T.stripSuffix "h" -> Just t) = parseDuration t * 3600
parseDuration (T.stripSuffix "d" -> Just t) = parseDuration t * 86400
parseDuration t = read $ T.unpack t

parseDist :: T.Text -> Double
parseDist (T.stripSuffix "km" -> Just t) = parseDist t * 1000
parseDist t = read $ T.unpack t

parseUTCTime :: T.Text -> UTCTime
parseUTCTime t =
    maybe (error $ "parseUTCTime " ++ T.unpack t) id $ parseUTCTime' t

parseUTCTime' :: T.Text -> Maybe UTCTime
parseUTCTime' t =
    parse (iso8601DateFormat Nothing) <|>
    parse (iso8601DateFormat (Just "%H:%M:%S")) <|>
    parse (iso8601DateFormat (Just "%H:%M:%SZ")) <|>
    parse (iso8601DateFormat (Just "%H:%M:%S%z")) <|>
    empty
    where
        parse f = parseTimeM False defaultTimeLocale f (T.unpack t)


-- Helpers --

distinctUsing :: (Ord b) => (a -> b) -> [a] -> [a]
distinctUsing f = go Set.empty
    where
        go _ [] = []
        go seen (x:xs) =
            let x' = f x
            in if x' `Set.member` seen
                then go seen xs
                else x : go (x' `Set.insert` seen) xs


fromRightM :: (Monad m, Show a) => m (Either a b) -> m b
fromRightM = fmap (either (error . show) id)

data UpsertResult rec
    = UpsertAdded (Entity rec)
    | UpsertDeleted (Key rec)
    | UpsertUpdated (Entity rec)
    | UpsertNoop (Entity rec)

deriving instance (Show (Entity rec), Show (Key rec)) => Show (UpsertResult rec)

uprepsert :: (Eq rec, Eq (Unique rec),
              PersistEntity rec, PersistEntityBackend rec ~ SqlBackend)
          => rec -> SqlPersistM (UpsertResult rec)
uprepsert rec =
    insertBy rec >>= \case
        Left dup -> do
            if entityVal dup /= rec
                then do
                    Nothing <- replaceUnique (entityKey dup) rec
                    return $ UpsertUpdated dup
                else
                    return $ UpsertNoop dup
        Right key ->
            return $ UpsertAdded $ Entity key rec

delEntities :: (PersistEntity rec, PersistEntityBackend rec ~ SqlBackend)
            => [UpsertResult rec] -> SqlPersistM [UpsertResult rec]
delEntities res = do
    allKeys <- selectKeysList [] []
    let delKeys = allKeys \\ map entityKey (keptEntities res)
    mapM (\k -> deleteKey k >> return (UpsertDeleted k)) delKeys

keptEntities :: [UpsertResult rec] -> [Entity rec]
keptEntities = concatMap $ \case
    UpsertAdded   e -> [e]
    UpsertDeleted _ -> []
    UpsertUpdated e -> [e]
    UpsertNoop    e -> [e]

changedEntities :: [UpsertResult rec] -> [Key rec]
changedEntities = concatMap $ \case
    UpsertAdded   e -> [entityKey e]
    UpsertDeleted k -> [k]
    UpsertUpdated e -> [entityKey e]
    UpsertNoop    _ -> []

syncEntities :: (Eq rec, Eq (Unique rec),
                 PersistEntity rec, PersistEntityBackend rec ~ SqlBackend)
             => [rec] -> SqlPersistM [UpsertResult rec]
syncEntities = mapM uprepsert

syncEntitiesDel :: (Eq rec, Eq (Unique rec),
                    PersistEntity rec, PersistEntityBackend rec ~ SqlBackend)
                => [rec] -> SqlPersistM [UpsertResult rec]
syncEntitiesDel recs = do
    syncRes <- syncEntities recs
    delRes <- delEntities syncRes
    return $ syncRes ++ delRes

wipeInsertMany :: forall rec.
                  (PersistEntity rec, PersistEntityBackend rec ~ SqlBackend)
               => Maybe [Key rec] -> [rec] -> SqlPersistM ()
wipeInsertMany old recs = do
    delete $ from $ \row -> case old of
        Nothing -> return ()
        Just olds -> where_ $ row ^. persistIdField `in_` valList olds
    -- FIXME: https://github.com/yesodweb/persistent/issues/527
    mapM_ insertMany_ (chunksOf 100 recs)

(!) :: (Ord k, Show k, Show a) => Map.Map k a -> k -> a
m ! v = unsafePerformIO $ try (evaluate (m Map.! v)) >>= \case
    Right x -> return x
    Left (e :: SomeException) ->
        error $ show m ++ " ! " ++ show v ++ ": " ++ show e

-- | Avoids the performance penalty of parsing into UTCTime
castToPersistValue :: SqlExpr (Value a) -> SqlExpr (Value PersistValue)
castToPersistValue = veryUnsafeCoerceSqlExprValue
