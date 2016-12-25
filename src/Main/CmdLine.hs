module Main.CmdLine (main) where

import Protolude

import qualified Strive as S (Client, buildClient)

import Config (token)
import StravaGear.Sync (sync)
import StravaGear.Report (report)


testClient :: IO S.Client
testClient = S.buildClient $ Just $ toS token

main :: IO ()
main = testClient >>= sync False >>= report
