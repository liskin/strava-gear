module Main.CmdLine (main) where

import Strava

main :: IO ()
main = testClient >>= sync False >>= report
