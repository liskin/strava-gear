{-# LANGUAGE OverloadedStrings #-}

module StravaGear.ConfigSpec (spec) where

import Protolude

import qualified Data.Set as S (fromList)
import Data.Time (UTCTime(UTCTime), fromGregorian)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

import StravaGear.Config.Internal
    ( Conf(..)
    , Symbol(..)
    , parseConf', time
    )
import StravaGear.Types (ComponentText(..), RoleText(..))

spec :: Spec
spec = do
    spec_conf
    spec_time

spec_conf :: Spec
spec_conf = describe "parseConf'" $ do
    let p = parseConf' known
    it "parses empty files" $ do
        p "" `shouldParse` []
        p " " `shouldParse` []
        p "\n" `shouldParse` []
    it "ignores comments" $ do
        p "-- comment" `shouldParse` []
        p "-- comment\n" `shouldParse` []
        p "--a\n--b\n" `shouldParse` []
    it "parses role" $ do
        p "role chain" `shouldParse` [ConfRole "chain"]
        p "role r1\nrole r2" `shouldParse` [ConfRole "r1", ConfRole "r2"]
        p `shouldFailOn` "role"
        p `shouldFailOn` "role -- x"
    it "parses roles" $ do
        p "roles\n chain" `shouldParse` [ConfRole "chain"]
        p "roles\n r1\n r2" `shouldParse` [ConfRole "r1", ConfRole "r2"]
        p "roles\n r1 -- c1\n r2" `shouldParse` [ConfRole "r1", ConfRole "r2"]
        p "roles\n r1 \n -- c1\n r2" `shouldParse` [ConfRole "r1", ConfRole "r2"]
        p `shouldFailOn` "roles"
        p `shouldFailOn` "roles -- x"
    it "parses component" $ do
        p "component x y 0 0" `shouldParse`
            [ConfComponent "x" "y" 0 0]
        p "component x y" `shouldParse`
            [ConfComponent "x" "y" 0 0]
        p "component chain1 CN-HG-53 1h 1km" `shouldParse`
            [ConfComponent "chain1" "CN-HG-53" 3600 1000]
        p "component chain2 \"x y\" 0 0.5km" `shouldParse`
            [ConfComponent "chain2" "x y" 0 500]
    it "parses components" $ do
        p "components\n c1 desc" `shouldParse` [ConfComponent "c1" "desc" 0 0]
    it "parses bike" $ do
        p "bike b\n chain c1 2016-01-01" `shouldParse`
            [ConfLongterm "b" "chain" "c1" t0 Nothing]
        p `shouldFailOn` "bike"
        p `shouldFailOn` "bike b"
        p `shouldFailOn` "bike b chain c1 2016-01-01"
        p `shouldFailOn` "bike b\nchain c1 2016-01-01"
        p `shouldFailOn` "bike b\n cain c1 2016-01-01"
        p `shouldFailOn` "bike b\n chain c2 2016-01-01"
    it "parses hashtag" $ do
        p "hashtag #b\n chain c1 2016-01-01" `shouldParse`
            [ConfHashTag "#b" "chain" "c1" t0 Nothing]
        p `shouldFailOn` "hashtag"
        p `shouldFailOn` "hashtag #b"
        p `shouldFailOn` "hashtag #b chain c1 2016-01-01"
        p `shouldFailOn` "hashtag #b\nchain c1 2016-01-01"
        p `shouldFailOn` "hashtag b\n chain c1 2016-01-01"
        p `shouldFailOn` "hashtag \"#b\"\n chain c1 2016-01-01"
        p `shouldFailOn` "hashtag #b\n cain c1 2016-01-01"
        p `shouldFailOn` "hashtag #b\n chain c2 2016-01-01"
  where
    t0 = UTCTime (fromGregorian 2016 1 1) 0
    known = S.fromList
        [ Symbol (RoleText "chain")
        , Symbol (ComponentText "c1")
        ]

spec_time :: Spec
spec_time = describe "time" $ do
    let p = parse time ""
    it "parses date" $
        p "2016-01-01" `shouldParse` t0
    it "parses date/time" $
        p "2016-01-01T01:00:00" `shouldParse` t1
    it "parses utc date/time" $
        p "2016-01-01T01:00:00Z" `shouldParse` t1
    it "parses positive time zone" $
        p "2016-01-01T01:00:00+01:00" `shouldParse` t0
    it "parses negative time zone" $
        p "2016-01-01T00:00:00-01:00" `shouldParse` t1
  where
    t0 = UTCTime (fromGregorian 2016 1 1) 0
    t1 = UTCTime (fromGregorian 2016 1 1) 3600
