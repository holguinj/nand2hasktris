{-# LANGUAGE RecordWildCards #-}
module Parse where

import           Prelude             hiding (maybe)

import           Control.Applicative ((<|>))
import           Control.Monad.State (StateT, get, lift, put, runStateT)
import           Data.Char           as Char

type Parser a = StateT ParserState ParseResult a

type ParseError = String
type ParseResult = Either ParseError

data ParserState = ParserState { remaining :: String
                               , original  :: String
                               , path      :: FilePath
                               } deriving (Show)

data ErrorState = ErrorState { lineNumber :: Int
                             , column     :: Int
                             , annotated  :: Maybe (String, String)
                             }

underline :: String -> Int -> (String, String)
underline line pos =
  let squiggles = replicate pos '~'
      underline' = squiggles ++ "^"
      diff = length line - length underline'
      tail = replicate diff ' '
   in (line, underline' ++ tail)

errorState :: ParserState -> ErrorState
errorState ParserState{..} =
  let allLines = lines original
      totalLength = length original
      doneLength = totalLength - length remaining
      parsed = take doneLength original
      parsedLines = lines parsed
      currentLine = case parsedLines of
                      [] -> ""
                      _  -> last parsedLines
      currentColumn = length currentLine
      doneLineLength = length parsedLines
      originalLine =  allLines !! (doneLineLength - 1)
      annotated' = if null currentLine
                     then Nothing
                     else Just $ underline originalLine currentColumn
    in ErrorState (length parsedLines - 1) currentColumn annotated'

formatError :: String -> ParserState -> String
formatError msg ps@ParserState{..} =
  let ErrorState{..} = errorState ps
      location = "(" ++ path ++ ":" ++ show lineNumber ++ ":" ++ show column ++ ")"
  in
    case annotated of
      Just (original, underlined) -> msg ++ ":\n\t" ++ original ++ "\n\t" ++ underlined
                                     ++ "\n" ++ location
      _ -> msg ++ location

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
replParse p s = runStateT p (ParserState s s "REPL")

noParse :: String -> Parser a
noParse msg = do
  state <- get
  lift $ Left $ formatError msg state

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
digits = parseWhile "digit(s)" (`elem` "0123456789")

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
whitespace = parseWhile "whitespace" (`elem` " \t\n") >> return ()

comma = char ',' <|> noParse "Expected comma"

threeNums :: Parser (String, String, String)
threeNums = do
  x <- digits
  comma
  whitespace
  y <- digits
  comma
  whitespace
  z <- digits
  eof
  return (x, y, z)

testError :: Parser a -> String -> IO ()
testError p s = do
  let Left err = replParse p s in
    putStrLn err
