{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module StravaGear.Config.SampleSpec (spec) where

import Protolude

import Test.Hspec
import Text.Heredoc

import StravaGear.Config.Sample (mkSampleConfig)
import StravaGear.Database.Schema (Bike(..))


spec :: Spec
spec = describe "mkSampleSpec" $ do
    it "works with empty db" $
        mkSampleConfig [] `shouldBe`
            [str|roles
                |  chain
                |  tire_front
                |  tire_rear
                |
                |-- no bikes in database :-(
                |]

    it "works with some bikes" $
        mkSampleConfig
            [ Bike{ bikeName = "city", bikeStravaId = "b123" }
            , Bike{ bikeName = "road", bikeStravaId = "b456" }
            ]
            `shouldBe`
            [str|roles
                |  chain
                |  tire_front
                |  tire_rear
                |
                |components
                |  c1 "SH CN-HG-XXX"
                |  c2 "SH CN-HG-XXX"
                |
                |bike b123 -- city
                |  chain c1 2000-01-01
                |
                |bike b456 -- road
                |  chain c2 2000-01-01
                |]
