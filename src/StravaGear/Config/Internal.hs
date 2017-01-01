{-# LANGUAGE OverloadedStrings #-}

module StravaGear.Config.Internal
    ( Conf(..)
    , parseConf

    -- * testing
    , parseConf'
    , time
    , toSymbol
    )
  where

import Protolude hiding (isPrefixOf, try)

import Control.Monad.Fail (MonadFail(fail))
import qualified Data.Set as S (member, singleton)
import Data.String (String)

import Data.Scientific (toRealFloat)
import Data.Time (UTCTime, defaultTimeLocale, iso8601DateFormat, parseTimeM)
import Text.Megaparsec
    ( Dec
    , ParsecT
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
    , runParserT
    , parseErrorPretty
    , spaceChar
    , string
    )
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

import StravaGear.Types
    ( BikeText(BikeText)
    , ComponentText(ComponentText)
    , HashTagText(HashTagText)
    , RoleText(RoleText)
    )


data Conf
    = ConfComponent !ComponentText !Text !Int !Double
    | ConfRole !RoleText
    | ConfLongterm !BikeText !RoleText !ComponentText !UTCTime !(Maybe UTCTime)
    | ConfHashTag !HashTagText !RoleText !ComponentText !UTCTime !(Maybe UTCTime)
  deriving (Eq, Ord, Show)

parseConf :: Text -> Either Text [Conf]
parseConf = first (toS . parseErrorPretty) . parseConf' mempty

parseConf' :: KnownSymbols -> Text -> ParseResult [Conf]
parseConf' = runParser conf


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
    role' = ConfRole <$> declareKnown roleText

component :: Parser [Conf]
component = indentBlock $
        keyword "components" *> indentSome component'
    <|> keyword "component" *> indentNone component'
  where
    component' =
        ConfComponent <$> declareKnown componentText <*> word
            <*> option 0 duration <*> option 0 distance

bike :: Parser [Conf]
bike = indentBlock $ do
    b <- keyword "bike" *> bikeText -- TODO: check bike
    indentSome $
        ConfLongterm <$> pure b
            <*> checkKnown roleText
            <*> checkKnown componentText
            <*> time <*> optional time

hashtag :: Parser [Conf]
hashtag = indentBlock $ do
    t <- keyword "hashtag" *> hashTagText
    indentSome $
        ConfHashTag <$> pure t
            <*> checkKnown roleText
            <*> checkKnown componentText
            <*> time <*> optional time


------------------------------------------------------------------------------
-- Lexer

lineComment :: ParserT m ()
lineComment = L.skipLineComment "--"

scn :: ParserT m ()
scn = L.space (void spaceChar) lineComment empty

lexeme :: ParserT m a -> ParserT m a
lexeme = L.lexeme sc . followedBySpace
  where
    sc = L.space (void $ oneOf [' ', '\t']) lineComment empty
    followedBySpace = (<* (lookAhead (void spaceChar) <|> eof))

keyword :: String -> ParserT m ()
keyword s = void $ lexeme $ string s

word :: ParserT m Text
word = lexeme $ toS <$> (stringLiteral <|> some c)
  where
    c = alphaNumChar <|> oneOf ['-','_','#']
    stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

duration :: ParserT m Int
duration = label "duration" $
    lexeme $ (*) <$> int <*> option 1 (hours <|> days)
  where
    int = fromIntegral <$> L.integer
    hours = string "h" *> pure 3600
    days = string "d" *> pure 86400

distance :: ParserT m Double
distance = label "distance" $
    lexeme $ (*) <$> double <*> option 1 kms
  where
    double = toRealFloat <$> L.number
    kms = string "km" *> pure 1000

time :: ParserT m UTCTime
time = label "date/time" $ lexeme $ do
    s <- some c
    p Nothing s
        <|> p (Just "%H:%M:%S") s
        <|> p (Just "%H:%M:%SZ") s
        <|> p (Just "%H:%M:%S%z") s
  where
    c = numberChar <|> oneOf ['T', 'Z', '+', '-', ':']
    p = parseTimeM False defaultTimeLocale . iso8601DateFormat

hashTag :: ParserT m Text
hashTag = lexeme $ toS <$> ((:) <$> char '#' <*> some c)
  where
    c = alphaNumChar <|> oneOf ['-','_','#']

bikeText :: Parser BikeText
bikeText = BikeText <$> word

componentText :: Parser ComponentText
componentText = ComponentText <$> word

hashTagText :: Parser HashTagText
hashTagText = HashTagText <$> hashTag

roleText :: Parser RoleText
roleText = RoleText <$> word


------------------------------------------------------------------------------
-- Indentation-sensitive helpers

nonIndented :: ParserT m a -> ParserT m a
nonIndented = L.nonIndented scn

indentBlock :: ParserT m (L.IndentOpt (ParserT m) a b) -> ParserT m a
indentBlock = L.indentBlock scn

indentNone :: ParserT m a -> ParserT m (L.IndentOpt (ParserT m) [a] b)
indentNone p = L.IndentNone <$> pure <$> p <* scn
    -- the "<* scn" belongs to L.indentBlock instead :-(
    -- (fixed in megaparsec 5.2.0)

indentSome :: ParserT m a -> ParserT m (L.IndentOpt (ParserT m) [a] a)
indentSome = pure . L.IndentSome Nothing pure


------------------------------------------------------------------------------
-- Parser monad

data Symbol
    = SymbolRole !RoleText
    | SymbolComponent !ComponentText
  deriving (Eq, Ord, Show)

class Show a => SymbolLike a where toSymbol :: a -> Symbol
instance SymbolLike RoleText where toSymbol = SymbolRole
instance SymbolLike ComponentText where toSymbol = SymbolComponent

type KnownSymbols = Set Symbol

declareKnown :: SymbolLike s => Parser s -> Parser s
declareKnown s = do
    x <- s
    modify (<> S.singleton (toSymbol x))
    pure x

checkKnown :: SymbolLike s => Parser s -> Parser s
checkKnown s = do
    x <- s
    whenM (gets $ not . S.member (toSymbol x)) $
        fail $ "undeclared " <> show x
    pure x

type ParserT = ParsecT Dec Text
type Parser = ParserT (State KnownSymbols)
type ParseResult = Either (ParseError Char Dec)

runParser :: Parser a -> KnownSymbols -> Text -> ParseResult a
runParser p s = flip evalState s . runParserT p "" . (<> "\n")
    -- comments and indentation-sensitive parsing wreak havoc if the
    -- input doesn't end with a newline
