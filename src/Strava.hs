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
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Database.Esqueleto as E
import qualified Database.Esqueleto.Internal.Sql as E (veryUnsafeCoerceSqlExprValue)
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
        tag T.Text
        component ComponentId
        role ComponentRoleId
        UniqueHashTagBikeComponent tag component role
        deriving Eq Ord Show
|]

deriving instance Eq (Unique Bike)
deriving instance Eq (Unique Component)
deriving instance Eq (Unique ComponentRole)
deriving instance Eq (Unique Activity)
deriving instance Eq (Unique LongtermBikeComponent)
deriving instance Eq (Unique HashTagBikeComponent)


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
        syncActivitiesComponents $ activitiesToRefresh changed
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

syncActivitiesComponents :: Maybe [Key Activity] -> SqlPersistM ()
syncActivitiesComponents new = do
    l <- activitiesLongtermComponents new
    h <- activitiesHashtagComponents new
    let dedup = distinctOn $ activityComponentActivity &&& activityComponentRole
    old <- mapM activityActivityComponents new
    wipeInsertMany old $ dedup $ h ++ l

activityActivityComponents :: [Key Activity] -> SqlPersistM [Key ActivityComponent]
activityActivityComponents as =
    fmap (map (\(E.Value v) -> v)) $ E.select $ E.from $ \ac -> do
        E.where_ $ ac E.^. ActivityComponentActivity `E.in_` E.valList as
        return $ ac E.^. ActivityComponentId

activitiesLongtermComponents :: Maybe [Key Activity] -> SqlPersistM [ActivityComponent]
activitiesLongtermComponents new = do
    let f (_, E.Value k, E.Value c, E.Value r) = ActivityComponent k c r
    fmap (map f) $ E.select $ E.from $ \(a `E.InnerJoin` b `E.InnerJoin` lt) -> do
        E.on $ a E.^. ActivityGearId E.==. E.just (b E.^. BikeStravaId)
        E.on $ b E.^. BikeId E.==. lt E.^. LongtermBikeComponentBike
        mapM_ (\n -> E.where_ $ a E.^. ActivityId `E.in_` E.valList n) new
        E.where_ $ lt E.^. LongtermBikeComponentStartTime E.<=. a E.^. ActivityStartTime
        E.where_ $ lt E.^. LongtermBikeComponentEndTime E.>=. E.just (a E.^. ActivityStartTime)
            E.||. E.isNothing (lt E.^. LongtermBikeComponentEndTime)
        E.groupBy (a E.^. ActivityId, lt E.^. LongtermBikeComponentRole)
        return ( castToPersistValue (E.max_ $ lt E.^. LongtermBikeComponentStartTime)
               , a E.^. ActivityId
               , lt E.^. LongtermBikeComponentComponent
               , lt E.^. LongtermBikeComponentRole)

activitiesHashtagComponents :: Maybe [Key Activity] -> SqlPersistM [ActivityComponent]
activitiesHashtagComponents new = do
    hashtags <- selectList [] []
    let tagMap = Map.fromListWith (++)
            [ ( hashTagBikeComponentTag h, [( hashTagBikeComponentComponent h
                                            , hashTagBikeComponentRole h )] )
            | Entity _ h <- hashtags ]
    let f (E.Value k, E.Value n) =
            [ ActivityComponent k c r
            | h <- nameHashTags n, (c, r) <- Map.findWithDefault [] h tagMap ]
    fmap (concatMap f) $ E.select $ E.from $ \a -> do
        mapM_ (\n -> E.where_ $ a E.^. ActivityId `E.in_` E.valList n) new
        return (a E.^. ActivityId, a E.^. ActivityName)

nameHashTags :: T.Text -> [T.Text]
nameHashTags name = [ w | w <- T.words name, "#" `T.isPrefixOf` w ]


-- Report --

report :: T.Text -> IO ()
report fileName = do
    runSqlite fileName $ do
        tab <- componentReport
        liftIO $ putStr $ Tab.render id id id tab
        return ()

componentReport :: SqlPersistM (Tab.Table String String String)
componentReport = do
    res <- E.select $
        E.from $ \(c `E.InnerJoin` ac `E.InnerJoin` a `E.InnerJoin` r) -> do
            E.on (c E.^. ComponentId E.==. ac E.^. ActivityComponentComponent)
            E.on (ac E.^. ActivityComponentActivity E.==. a E.^. ActivityId)
            E.on (ac E.^. ActivityComponentRole E.==. r E.^. ComponentRoleId)
            E.groupBy (c E.^. ComponentId)
            E.orderBy
                [ E.asc $ r E.^. ComponentRoleName
                , E.asc $ c E.^. ComponentUniqueId ]
            let iniTime = E.just $ c E.^. ComponentInitialSeconds
                iniDist = E.just $ c E.^. ComponentInitialMeters
                sumMovingTime = E.sum_ $ a E.^. ActivityMovingTime
                sumDist = E.sum_ $ a E.^. ActivityDistance
                firstUsage = E.min_ $ a E.^. ActivityStartTime
                lastUsage = E.max_ $ a E.^. ActivityStartTime
            return
                ( r, c
                , iniTime E.+. sumMovingTime
                , iniDist E.+. sumDist
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
                , E.Value (Just time), E.Value (Just dist)
                , E.Value (Just firstUsage), E.Value (Just lastUsage)
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
    let componentMap = Map.fromList
            [ (componentUniqueId v, k) | Entity k v <- keptEntities components ]
        roleMap = Map.fromList
            [ (componentRoleName v, k) | Entity k v <- keptEntities roles ]
        bikeMap = Map.fromList
            [ (bikeStravaId v, k) | Entity k v <- bikes ]
    longterms <- syncEntitiesDel
        [ LongtermBikeComponent (componentMap ! c)
                                (bikeMap ! b) (roleMap ! r) s e
        | ConfLongterm c b r s e <- cs ]
    hashtags <- syncEntitiesDel
        [ HashTagBikeComponent t (componentMap ! c) (roleMap ! r)
        | ConfHashTag t c r <- cs ]
    return (components, roles, longterms, hashtags)

data Conf
    = ConfComponent T.Text T.Text Int Double
    | ConfRole T.Text
    | ConfLongterm T.Text T.Text T.Text UTCTime (Maybe UTCTime)
    | ConfHashTag T.Text T.Text T.Text

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
    ["hashtag", tag, component, role] ->
        [ConfHashTag tag component role]
    err ->
        error $ show err

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

distinctOn :: (Ord b) => (a -> b) -> [a] -> [a]
distinctOn f = go Set.empty
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
    mapM (\k -> delete k >> return (UpsertDeleted k)) delKeys

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
    E.delete $ E.from $ \row -> case old of
        Nothing -> return ()
        Just olds -> E.where_ $ row E.^. persistIdField `E.in_` E.valList olds
    -- FIXME: https://github.com/yesodweb/persistent/issues/527
    mapM_ insertMany_ (chunksOf 100 recs)

(!) :: (Ord k, Show k, Show a) => Map.Map k a -> k -> a
m ! v = unsafePerformIO $ try (evaluate (m Map.! v)) >>= \case
    Right x -> return x
    Left (e :: SomeException) ->
        error $ show m ++ " ! " ++ show v ++ ": " ++ show e

-- | Avoids the performance penalty of parsing into UTCTime
castToPersistValue :: E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value PersistValue)
castToPersistValue = E.veryUnsafeCoerceSqlExprValue
