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
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import Data.List ((\\), nubBy)
import Data.List.Split (chunksOf)
import Data.Time
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Strive as S

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
        deriving Eq Ord Show
|]

deriving instance Eq (Unique Bike)
deriving instance Eq (Unique Component)
deriving instance Eq (Unique ComponentRole)
deriving instance Eq (Unique Activity)

testClient :: IO S.Client
testClient = S.buildClient (Just $ T.pack token)

sync :: S.Client -> IO ()
sync client = do
    Right athlete <- S.getCurrentAthlete client
    let athleteId = S.id `S.get` athlete :: Integer
        athleteBikes = S.bikes `S.get` athlete
        sqliteFileName = "athlete_" ++ show athleteId ++ ".sqlite"
        confFileName = "athlete_" ++ show athleteId ++ ".conf"
    print sqliteFileName
    conf <- T.readFile confFileName
    runSqlite (T.pack sqliteFileName) $ do
        runMigration migrateAll
        _ <- syncBikes athleteBikes
        _ <- syncActivities client -- FIXME: not always
        transactionSave
        syncConfig conf
        syncActivitiesComponents
        return ()

syncBikes :: [S.GearSummary] -> SqlPersistM [UpsertResult Bike]
syncBikes bikes = syncEntitiesDel $ map bike bikes
    where bike b = Bike (S.name `S.get` b) (S.id `S.get` b)

-- TODO: avoid refetching all activities every time
syncActivities :: S.Client -> SqlPersistM [UpsertResult Activity]
syncActivities client = do
    acts <- fetchActivities client
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
        fetch t =
            fromRightM $ liftIO $ S.getCurrentActivities client $
                S.with [ S.before `S.set` Just t ]
        go t = do
            liftIO $ putStrLn $ "fetching activities before " ++ show t
            acts <- fetch t
            if null acts
                then return [acts]
                else fmap (acts :) $ go (S.startDate `S.get` last acts)


syncActivitiesComponents :: SqlPersistM ()
syncActivitiesComponents = do
    acts <- selectList [ActivityGearId !=. Nothing] []
    actComponents <- mapM syncActivityComponents acts
    wipeInsertMany $ concat actComponents

syncActivityComponents :: Entity Activity -> SqlPersistM [ActivityComponent]
syncActivityComponents act = do
    let gearId = maybe (error "no gear id") id $ activityGearId $ entityVal act
    getBy (StravaBikeId gearId) >>= \case
        Nothing ->
            -- ignore retired bikes, we don't have them in database
            return []
        Just bike -> do
            longterms <- activityLongtermComponents bike act
            fromtags <- activityHashtagComponents act
            return $ nubBy ((==) `on` activityComponentRole) $ fromtags ++ longterms

activityLongtermComponents :: Entity Bike -> Entity Activity
                           -> SqlPersistM [ActivityComponent]
activityLongtermComponents bike (Entity k act) = do
    let longtermFilter =
            [ LongtermBikeComponentBike ==. entityKey bike
            , LongtermBikeComponentStartTime <=. activityStartTime act
            ] ++
            ([ LongtermBikeComponentEndTime >=. Just (activityStartTime act) ] ||.
             [ LongtermBikeComponentEndTime ==. Nothing ])
    longterms <- selectList longtermFilter []
    return [ ActivityComponent k
             (longtermBikeComponentComponent l)
             (longtermBikeComponentRole l)
           | Entity _ l <- longterms ]

activityHashtagComponents :: Entity Activity -> SqlPersistM [ActivityComponent]
activityHashtagComponents (Entity k act) = do
    let hashtagFilter = [ HashTagBikeComponentTag <-. actHashTags act ]
    fromtags <- selectList hashtagFilter []
    return [ ActivityComponent k
             (hashTagBikeComponentComponent h)
             (hashTagBikeComponentRole h)
           | Entity _ h <- fromtags ]

actHashTags :: Activity -> [T.Text]
actHashTags Activity{activityName = name} =
    [ w | w <- T.words name, "#" `T.isPrefixOf` w ]


-- Text config (to be replaced by REST) --

syncConfig :: T.Text -> SqlPersistM ()
syncConfig conf = do
    let ls = T.lines conf
        cs = concatMap parseConf ls
    components <- fmap keptEntities $ syncEntitiesDel
        [ Component c n dur dist | ConfComponent c n dur dist <- cs ]
    roles <- fmap keptEntities $ syncEntitiesDel
        [ ComponentRole n | ConfRole n <- cs ]
    bikes <- selectList [] []
    let componentMap = Map.fromList
            [ (componentUniqueId v, k) | Entity k v <- components ]
        roleMap = Map.fromList
            [ (componentRoleName v, k) | Entity k v <- roles ]
        bikeMap = Map.fromList
            [ (bikeStravaId v, k) | Entity k v <- bikes ]
    wipeInsertMany
        [ LongtermBikeComponent (componentMap ! c)
                                (bikeMap ! b) (roleMap ! r) s e
        | ConfLongterm c b r s e <- cs ]
    wipeInsertMany
        [ HashTagBikeComponent t (componentMap ! c) (roleMap ! r)
        | ConfHashTag t c r <- cs ]

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

distinct :: (Ord a) => [a] -> [a]
distinct = Set.toList . Set.fromList

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
               => [rec] -> SqlPersistM ()
wipeInsertMany recs = do
    deleteWhere ([] :: [Filter rec])
    -- FIXME: https://github.com/yesodweb/persistent/issues/527
    mapM_ insertMany_ (chunksOf 100 recs)

(!) :: (Ord k, Show k, Show a) => Map.Map k a -> k -> a
m ! v = unsafePerformIO $ try (evaluate (m Map.! v)) >>= \case
    Right x -> return x
    Left (e :: SomeException) ->
        error $ show m ++ " ! " ++ show v ++ ": " ++ show e
