module Main where

import Strava

main :: IO ()
main = testClient >>= sync >>= report
