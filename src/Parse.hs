module Parse where

import Prelude hiding (maybe)

import           Control.Applicative ((<|>))
import           Control.Monad.State (StateT, get, lift, put, runStateT)
import           Data.Char           as Char

type Parser a = StateT ParserState ParseResult a

type ParseError = String
type ParseResult = Either ParseError

data ParserState = MkState { remaining :: String
                           , original  :: String
                           , path      :: FilePath
                           } deriving (Show)

getString :: Parser String
getString = do
  state <- get
  return $ remaining state

putString :: String -> Parser ()
putString s = do
  state <- get
  put $ state { remaining = s }

parse :: Parser a -> ParserState -> ParseResult a
parse = fmap (fmap fst) . runStateT

replParse :: Parser a -> String -> ParseResult (a, ParserState)
replParse p s = runStateT p (MkState s s "REPL")

noParse :: String -> Parser a
noParse = lift . Left

parseIf :: (Char -> Bool) -> Parser Char
parseIf f = do
  state <- getString
  case state of
    [] -> noParse "No input remaining"
    (c:cs) -> if f c
                then do putString cs
                        return c
                else noParse $ "Unexpected " ++ show c

parseWhile :: String -> (Char -> Bool) -> Parser String
parseWhile description f = do
  state <- getString
  let (result, newState) = span f state
  if null result
    then noParse $ "Expected " ++ description
    else do putString newState
            return result

char :: Char -> Parser Char
char = parseIf . (==)

between :: (Char, Char) -> Parser a -> Parser a
between (start, end) p = do
  char start
  middle <- p
  char end
  return middle

inParens :: Parser a -> Parser a
inParens = between ('(', ')')

digits :: Parser String
digits = parseWhile "digits" (`elem` "0123456789")

eol :: Parser ()
eol = char '\n' >> return ()

eof :: Parser ()
eof = do
  state <- getString
  if null state
    then return ()
    else noParse "Expected end of file"

optional :: Parser a -> Parser ()
optional p = (p >> return ()) <|> return ()

maybe :: Parser a -> Parser (Maybe a)
maybe p = (p >>= return . Just) <|> return Nothing

whitespace :: Parser ()
whitespace = optional $ parseWhile "whitespace" (`elem` " \t\n")
