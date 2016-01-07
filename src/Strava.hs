{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Strava where

import Control.Monad.IO.Class (liftIO)
import Data.List ((\\))
import Data.Time
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Data.Text as T
import qualified Strive as S

import Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
    Bike
        name T.Text
        stravaId T.Text
        StravaId stravaId
        deriving Eq Show

    Component
        uniqueId T.Text
        UniqueId uniqueId
        name T.Text
        initialMeters Int
        initialSeconds Int
        deriving Eq Show
|]

deriving instance Eq (Unique Bike)

testClient :: IO S.Client
testClient = S.buildClient (Just $ T.pack token)

sync :: S.Client -> IO ()
sync client = do
    Right athlete <- S.getCurrentAthlete client
    let athleteId = S.id `S.get` athlete :: Integer
        athleteBikes = S.bikes `S.get` athlete
        fileName = "athlete_" ++ show athleteId ++ ".sqlite"
    print fileName
    runSqlite (T.pack fileName) $ do
        runMigration migrateAll
        syncBikesRes <- syncBikes athleteBikes
        liftIO $ print syncBikesRes
        --syncActivities client
        return ()

syncBikes :: [S.GearSummary] -> SqlPersistM [UpsertResult Bike]
syncBikes bikes = syncEntitiesDel $ map bike bikes
    where bike b = Bike (S.id `S.get` b) (S.name `S.get` b)

data UpsertResult rec
    = UpsertAdded (Key rec)
    | UpsertDeleted (Key rec)
    | UpsertUpdated (Key rec)
    | UpsertNoop (Key rec)

deriving instance Show (Key rec) => Show (UpsertResult rec)

uprepsert :: (Eq rec, Eq (Unique rec),
           PersistEntity rec, PersistEntityBackend rec ~ SqlBackend)
          => rec -> SqlPersistM (UpsertResult rec)
uprepsert rec =
    insertBy rec >>= \case
        Left dup -> do
            if entityVal dup /= rec
                then do
                    Nothing <- replaceUnique (entityKey dup) rec
                    return $ UpsertUpdated (entityKey dup)
                else
                    return $ UpsertNoop (entityKey dup)
        Right key ->
            return $ UpsertAdded key

delEntities :: (Eq rec, Eq (Unique rec),
                PersistEntity rec, PersistEntityBackend rec ~ SqlBackend)
               => [UpsertResult rec] -> SqlPersistM [UpsertResult rec]
delEntities res = do
    allKeys <- selectKeysList [] []
    let keepKeys = flip concatMap res $ \case
            UpsertAdded   k -> [k]
            UpsertDeleted _ -> []
            UpsertUpdated k -> [k]
            UpsertNoop    k -> [k]
        delKeys = allKeys \\ keepKeys
    mapM (\k -> delete k >> return (UpsertDeleted k)) delKeys

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

syncActivities :: S.Client -> SqlPersistM ()
syncActivities client = do
    getRes <- liftIO $ S.getCurrentActivities client $ S.with
        [ S.set S.after (Just (UTCTime (fromGregorian 1970 0 0) 0))
        , S.set S.perPage 201
        ]
        {-[ S.set S.before (Just (UTCTime (fromGregorian 1970 0 0) 0))
        , S.set S.after (Just (UTCTime (fromGregorian 1970 0 0) 0))
        , S.set S.page 1
        , S.set S.perPage 2
        ]-}
    case getRes of
        Right currentActivities ->
            liftIO $ print $ length currentActivities
        Left err ->
            liftIO $ print err
