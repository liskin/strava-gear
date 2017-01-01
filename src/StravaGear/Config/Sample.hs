{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

module StravaGear.Config.Sample
    ( mkSampleConfig
    , sampleConfig
    )
  where

import Protolude

import Data.Text (unlines)
import Database.Persist (SelectOpt(Asc), entityVal, selectList)
import Database.Persist.Sql (SqlPersistM)

import StravaGear.Database.Schema (Bike(..), EntityField(BikeStravaId))
import StravaGear.Types (fromBike)


sampleConfig :: SqlPersistM Text
sampleConfig =
    mkSampleConfig . map entityVal <$> selectList [] [Asc BikeStravaId]

mkSampleConfig :: [Bike] -> Text
mkSampleConfig bikes = unlines $
    [ "roles"
    , "  chain"
    , "  tire_front"
    , "  tire_rear"
    ] <> bikeConfs
  where
    chains = [ "c" <> show c | c <- [(1::Int)..] | _ <- bikes ]

    bikeConfs
        | null bikes =
            [ ""
            , "-- no bikes in database :-("
            ]
        | otherwise = chainConfs
            <> concatMap (uncurry bikeConf) (zip bikes chains)

    chainConfs =
          ""
        : "components"
        : map (\c -> "  " <> c <> " \"SH CN-HG-XXX\"") chains

    bikeConf Bike{..} chain =
        [ ""
        , "bike " <> fromBike bikeStravaId <> " -- " <> bikeName
        , "  chain " <> chain <> " 2000-01-01"
        ]
