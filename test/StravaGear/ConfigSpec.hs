{-# LANGUAGE OverloadedStrings #-}

module StravaGear.ConfigSpec (spec) where

import Protolude

import Test.Hspec

import StravaGear.Config (Conf(..), parseConf)

spec :: Spec
spec = describe "parseConf" $ do
    let p = parseConf
    it "parses empty files" $ do
        p "" `shouldBe` []
        p " " `shouldBe` []
        p "\n" `shouldBe` []
    it "ignores comments" $ do
        p "# comment" `shouldBe` []
        p "# comment\n" `shouldBe` []
        p "#a\n#b\n" `shouldBe` []
    it "parses roles" $ do
        p "role chain" `shouldBe` [ConfRole "chain"]
        p "role r1\nrole r2" `shouldBe` [ConfRole "r1", ConfRole "r2"]
    -- ...
