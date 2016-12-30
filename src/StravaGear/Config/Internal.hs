{-# LANGUAGE OverloadedStrings #-}
module StravaGear.Config.Internal
    ( Conf(..)
    , parseConf

    -- * testing
    , parseConf'
    , time
    )
  where

import Protolude hiding (isPrefixOf, try)

import Data.String (String)

import Data.Scientific (toRealFloat)
import Data.Time (UTCTime, defaultTimeLocale, iso8601DateFormat, parseTimeM)
import Text.Megaparsec
    ( Dec
    , ParseError
    , alphaNumChar
    , char
    , eof
    , label
    , lookAhead
    , manyTill
    , numberChar
    , oneOf
    , option
    , parse
    , parseErrorPretty
    , spaceChar
    , string
    )
import Text.Megaparsec.Text (Parser)
import qualified Text.Megaparsec.Lexer as L
    ( IndentOpt(..)
    , charLiteral
    , indentBlock
    , integer
    , lexeme
    , nonIndented
    , number
    , skipLineComment
    , space
    )


data Conf
    = ConfComponent Text Text Int Double
    | ConfRole Text
    | ConfLongterm Text Text Text UTCTime (Maybe UTCTime)
    | ConfHashTag Text Text Text UTCTime (Maybe UTCTime)
    deriving (Eq, Ord, Show)

parseConf :: Text -> Either Text [Conf]
parseConf = first (toS . parseErrorPretty) . parseConf'

type ParseResult = Either (ParseError Char Dec)

parseConf' :: Text -> ParseResult [Conf]
parseConf' = parse conf "" . (<> "\n")
    -- comments and indentation-sensitive parsing wreak havoc if the
    -- input doesn't end with a newline


------------------------------------------------------------------------------
-- Grammar

conf :: Parser [Conf]
conf = concat <$> (scn *> many topLevel <* eof)

topLevel :: Parser [Conf]
topLevel = nonIndented $
        role
    <|> component
    <|> bike
    <|> hashtag

role :: Parser [Conf]
role = indentBlock $
        keyword "roles" *> indentSome role'
    <|> keyword "role" *> indentNone role'
  where
    role' = ConfRole <$> word

component :: Parser [Conf]
component = indentBlock $
        keyword "components" *> indentSome component'
    <|> keyword "component" *> indentNone component'
  where
    component' =
        ConfComponent <$> word <*> word <*> option 0 duration <*> option 0 distance

bike :: Parser [Conf]
bike = indentBlock $ do
    b <- keyword "bike" *> word
    indentSome $
        ConfLongterm <$> pure b <*> word <*> word <*> time <*> optional time

hashtag :: Parser [Conf]
hashtag = indentBlock $ do
    t <- keyword "hashtag" *> word
    indentSome $
        ConfHashTag <$> pure t <*> word <*> word <*> time <*> optional time


------------------------------------------------------------------------------
-- Lexer

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

scn :: Parser ()
scn = L.space (void spaceChar) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc . followedBySpace
  where
    sc = L.space (void $ oneOf [' ', '\t']) lineComment empty
    followedBySpace = (<* (lookAhead (void spaceChar) <|> eof))

keyword :: String -> Parser ()
keyword s = void $ lexeme $ string s

word :: Parser Text
word = lexeme $ toS <$> (stringLiteral <|> some c)
  where
    c = alphaNumChar <|> oneOf ['-','_','#']
    stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

duration :: Parser Int
duration = label "duration" $
    lexeme $ (*) <$> int <*> option 1 (hours <|> days)
  where
    int = fromIntegral <$> L.integer
    hours = string "h" *> pure 3600
    days = string "d" *> pure 86400

distance :: Parser Double
distance = label "distance" $
    lexeme $ (*) <$> double <*> option 1 kms
  where
    double = toRealFloat <$> L.number
    kms = string "km" *> pure 1000

time :: Parser UTCTime
time = label "date/time" $ lexeme $ do
    s <- some c
    p Nothing s
        <|> p (Just "%H:%M:%S") s
        <|> p (Just "%H:%M:%SZ") s
        <|> p (Just "%H:%M:%S%z") s
  where
    c = numberChar <|> oneOf ['T', 'Z', '+', '-', ':']
    p = parseTimeM False defaultTimeLocale . iso8601DateFormat


------------------------------------------------------------------------------
-- Helpers

nonIndented :: Parser a -> Parser a
nonIndented = L.nonIndented scn

indentBlock :: Parser (L.IndentOpt Parser a b) -> Parser a
indentBlock = L.indentBlock scn

indentNone :: Parser a -> Parser (L.IndentOpt Parser [a] b)
indentNone p = L.IndentNone <$> pure <$> p <* scn
    -- the "<* scn" belongs to L.indentBlock instead :-(
    -- (fixed in megaparsec 5.2.0)

indentSome :: Parser a -> Parser (L.IndentOpt Parser [a] a)
indentSome = pure . L.IndentSome Nothing pure
