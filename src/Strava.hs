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
{-# OPTIONS_GHC -Wall #-}

module Strava where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
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
        syncBikes athleteBikes
        syncActivities client

syncBikes :: [S.GearSummary] -> SqlPersistM ()
syncBikes bikes = do
    forM_ bikes $ \bike -> do
        let bId = S.id `S.get` bike
            bName = S.name `S.get` bike
        uprepsert $ Bike bId bName
    -- TODO: delete removed bikes

uprepsert :: (Eq rec, Eq (Unique rec),
           PersistEntity rec, PersistEntityBackend rec ~ SqlBackend)
          => rec -> SqlPersistM ()
uprepsert rec =
    insertBy rec >>= \case
        Left dup -> do
            when (entityVal dup /= rec) $ do
                Nothing <- replaceUnique (entityKey dup) rec
                -- TODO: mark as changed
                return ()
        Right _key ->
            -- TODO: mark as added
            return ()

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
