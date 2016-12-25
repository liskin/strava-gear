{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module StravaGear.Config
    ( Conf(..)
    , parseConf
    )
  where

import Control.Applicative (Alternative(..))
import Data.Text (Text)
import qualified Data.Text as T (isPrefixOf, stripSuffix, unpack, words)
import Data.Time (UTCTime, defaultTimeLocale, iso8601DateFormat, parseTimeM)


data Conf
    = ConfComponent Text Text Int Double
    | ConfRole Text
    | ConfLongterm Text Text Text UTCTime (Maybe UTCTime)
    | ConfHashTag Text Text Text UTCTime (Maybe UTCTime)

parseConf :: Text -> [Conf]
parseConf l = case T.words l of
    comment:_ | "#" `T.isPrefixOf` comment -> []
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
        error $ "malformed config line: " ++ T.unpack l

parseDuration :: Text -> Int
parseDuration (T.stripSuffix "h" -> Just t) = parseDuration t * 3600
parseDuration (T.stripSuffix "d" -> Just t) = parseDuration t * 86400
parseDuration t = read $ T.unpack t

parseDist :: Text -> Double
parseDist (T.stripSuffix "km" -> Just t) = parseDist t * 1000
parseDist t = read $ T.unpack t

parseUTCTime :: Text -> UTCTime
parseUTCTime t =
    maybe (error $ "parseUTCTime " ++ T.unpack t) id $ parseUTCTime' t

parseUTCTime' :: Text -> Maybe UTCTime
parseUTCTime' t =
    parse (iso8601DateFormat Nothing) <|>
    parse (iso8601DateFormat (Just "%H:%M:%S")) <|>
    parse (iso8601DateFormat (Just "%H:%M:%SZ")) <|>
    parse (iso8601DateFormat (Just "%H:%M:%S%z")) <|>
    empty
    where
        parse f = parseTimeM False defaultTimeLocale f (T.unpack t)
