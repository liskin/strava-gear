{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module StravaGear.Config
    ( Conf(..)
    , parseConf
    )
  where

import Protolude hiding (isPrefixOf)

import Data.Text (lines, words, isPrefixOf, stripSuffix)
import Data.Time (UTCTime, defaultTimeLocale, iso8601DateFormat, parseTimeM)


data Conf
    = ConfComponent Text Text Int Double
    | ConfRole Text
    | ConfLongterm Text Text Text UTCTime (Maybe UTCTime)
    | ConfHashTag Text Text Text UTCTime (Maybe UTCTime)
    deriving (Eq, Ord, Show)

parseConf :: Text -> [Conf]
parseConf = concatMap parseConfLine . lines

parseConfLine :: Text -> [Conf]
parseConfLine l = case words l of
    comment:_ | "#" `isPrefixOf` comment -> []
    [] -> []
    ["component", code, name, iniDur, iniDist] ->
        [ConfComponent code name (parseDuration iniDur) (parseDist iniDist)]
    ["role", name] ->
        [ConfRole name]
    ["longterm", component, bike, role, start, end] ->
        [ConfLongterm component bike role (parseUTCTime start)
                                          (Just $ parseUTCTime end)]
    ["longterm", component, bike, role, start] ->
        [ConfLongterm component bike role (parseUTCTime start) Nothing]
    ["hashtag", tag, component, role, start, end] ->
        [ConfHashTag tag component role (parseUTCTime start)
                                        (Just $ parseUTCTime end)]
    ["hashtag", tag, component, role, start] ->
        [ConfHashTag tag component role (parseUTCTime start) Nothing]
    _ ->
        panic $ "malformed config line: " <> l

parseDuration :: Text -> Int
parseDuration (stripSuffix "h" -> Just t) = parseDuration t * 3600
parseDuration (stripSuffix "d" -> Just t) = parseDuration t * 86400
parseDuration (readMaybe . toS -> Just t) = t
parseDuration s = panic $ "malformed duration: " <> s

parseDist :: Text -> Double
parseDist (stripSuffix "km" -> Just t) = parseDist t * 1000
parseDist (readMaybe . toS -> Just t) = t
parseDist s = panic $ "malformed dist: " <> s

parseUTCTime :: Text -> UTCTime
parseUTCTime t =
    maybe (panic $ "parseUTCTime " <> t) identity $ parseUTCTime' t

parseUTCTime' :: Text -> Maybe UTCTime
parseUTCTime' t =
    parse (iso8601DateFormat Nothing) <|>
    parse (iso8601DateFormat (Just "%H:%M:%S")) <|>
    parse (iso8601DateFormat (Just "%H:%M:%SZ")) <|>
    parse (iso8601DateFormat (Just "%H:%M:%S%z")) <|>
    empty
    where
        parse f = parseTimeM False defaultTimeLocale f (toS t)
