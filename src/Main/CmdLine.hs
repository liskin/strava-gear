module Main.CmdLine (main) where

import qualified Data.Text as T (pack)
import qualified Strive as S (Client, buildClient)

import Config (token)
import StravaGear.Sync (sync)
import StravaGear.Report (report)


testClient :: IO S.Client
testClient = S.buildClient $ Just $ T.pack token

main :: IO ()
main = testClient >>= sync False >>= report
