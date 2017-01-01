{-# LANGUAGE OverloadedStrings #-}

module Main.CmdLine (main) where

import Protolude

import Database.Persist.Sqlite (runSqlite)
import qualified Strive as S
    ( Client
    , buildClient
    , get
    , getCurrentAthlete
    , id
    )
import qualified Text.Tabular.AsciiArt as Tab (render)

import Config (token)
import StravaGear.Config (parseConf)
import StravaGear.Sync (sync)
import StravaGear.Report (bikesReport, componentReport)


main :: IO ()
main = sync' False >>= report

testClient :: IO S.Client
testClient = S.buildClient $ Just $ toS token

report :: Text -> IO ()
report fileName = do
    runSqlite fileName $ do
        tab1 <- componentReport
        tab2 <- bikesReport
        liftIO $ do
            putStr $ Tab.render identity identity identity tab1
            putText ""
            putStr $ Tab.render identity identity identity tab2

sync' :: Bool -> IO Text
sync' forceFetch = do
    client <- testClient
    Right athlete <- S.getCurrentAthlete client
    let athleteId = S.id `S.get` athlete :: Integer
    let sqliteFileName = "athlete_" <> show athleteId <> ".sqlite"
    let confFileName = "athlete_" <> show athleteId <> ".conf"
    Right conf <- parseConf <$> readFile confFileName
    runSqlite sqliteFileName $ sync forceFetch conf client
    pure sqliteFileName
