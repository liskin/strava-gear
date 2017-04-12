{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main.CmdLine (main) where

import Protolude

import Database.Persist.Sql (SqlPersistM, runMigration)
import Database.Persist.Sqlite (runSqlite)
import qualified Strive as S
    ( buildClient
    , get
    , getCurrentAthlete
    , id
    )
import qualified Text.Tabular.AsciiArt as Tab (render)

import Config (token)
import StravaGear.Config (parseConf)
import StravaGear.Database.Schema (migrateAll)
import StravaGear.Sync (syncConfig, syncStrava)
import StravaGear.Report (bikesReport, componentReport)


main :: IO ()
main = do
    client <- S.buildClient $ Just $ toS token
    Right athlete <- S.getCurrentAthlete client
    let athleteId = S.id `S.get` athlete :: Integer
    let sqliteFileName = "athlete_" <> show athleteId <> ".sqlite"
    let confFileName = "athlete_" <> show athleteId <> ".conf"

    conf <- parseConf <$> readFile confFileName >>= \case
        Right conf -> pure conf
        Left e -> panic e
    runSqlite sqliteFileName $ do
        runMigration migrateAll
        void $ syncStrava False client
        syncConfig conf
        report

report :: SqlPersistM ()
report = do
    tab1 <- componentReport
    tab2 <- bikesReport
    liftIO $ do
        putStr $ Tab.render identity identity identity tab1
        putText ""
        putStr $ Tab.render identity identity identity tab2
