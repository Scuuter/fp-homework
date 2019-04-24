{-# LANGUAGE ScopedTypeVariables #-}

module Parser
  (
    Command(..)
  , Value(..)
  , Ref(..)
  , parseCommand
  )
where

import Data.Char (isDigit, isPrint, isSpace)
import Control.Applicative (liftA2, liftA3)

import Data.Maybe (fromMaybe, isNothing)
import Text.Read (readMaybe)
import Data.Void (Void)

import Text.Megaparsec (
    Parsec
  , empty
  , many
  , some
  , takeWhileP
  , (<|>)
  , eof
  , optional
  , choice
  , skipSome
  , satisfy
  )

import Text.Megaparsec.Char (
    char
  , eol
  , digitChar
  , letterChar
  , string
  )


data Command =
    Assign String Quote
  | Read [String]
  | Echo (Maybe NOption) Quote
  | Pwd
  | Cd FilePath
  | Exit (Maybe Int)
  deriving (Show)

data NOption = NOption
  deriving (Show)

data Value = Str String | Ref Ref
  deriving (Show, Eq)

data Ref = Arg Int | Var String
  deriving (Show, Eq)

type Quote = [Value]

type Parser = Parsec Void String


ws :: Parser String
ws = takeWhileP Nothing (liftA2 (||) (==' ') (=='\t'))

allSpaces :: Parser String
allSpaces = takeWhileP Nothing isSpace

nonEmpty :: (Monoid a, Eq a) => Parser a -> Parser a
nonEmpty p = do
  res <- p
  if res == mempty
    then empty
    else return res

word :: Parser String
word = do
  part <- takeWhileP Nothing (liftA3 (\a b c -> a && b && c) isPrint (not <$> isSpace) (not <$> isCommandSymbol))
  rest <- optional (liftA2 (:) commandSymbol word)
  return (part ++ fromMaybe "" rest)


isCommandSymbol :: Char -> Bool
isCommandSymbol c = (c == '$') || (c == '\\') || (c == '\"') || (c == '(') || (c == ')')

commandSymbol :: Parser Char
commandSymbol = char '\\' *> satisfy isCommandSymbol

parseCommand :: Parser Command
parseCommand = optional (ws *> skipSome eol) *> (parseEnd <|> choice
  [
    parseEcho
  , parseRead
  , parsePwd
  , parseCd
  , parseExit
  , parseAssign
  ]
  )

parseAssign :: Parser Command
parseAssign = do
  var <- parseVar
  _ <- char '='
  str <- many parseQuote
  return $ Assign var $ concat str

parseEcho :: Parser Command
parseEcho = do
  _ <- string "echo"
  _ <- nonEmpty ws
  n <- optional (string "-n" <* nonEmpty ws)
  let noption = if isNothing n then Nothing else Just NOption
  str <- many (ws *> parseQuote)
  return $ Echo noption (concat str)

parseRead :: Parser Command
parseRead = do
  _ <- string "read"
  vars <- many (nonEmpty ws *> parseVar)
  return $ Read vars

parsePwd :: Parser Command
parsePwd = do
  _ <- string "pwd"
  return Pwd

parseCd :: Parser Command
parseCd = do
  _ <- string "cd"
  _ <- nonEmpty ws
  path <- takeWhileP Nothing (/= '\n')
  return $ Cd path

parseExit :: Parser Command
parseExit = do
  _ <- string "exit"
  code <- optional (nonEmpty ws *> takeWhileP Nothing isDigit)
  return $ Exit (read <$> code)

parseQuote :: Parser Quote
parseQuote = choice
  [
    pure . Str <$> singleQuote
  , doubleQuote
  , pure . Ref <$> parseRef
  , pure . Str <$> nonEmpty word
  ]

parseRef :: Parser Ref
parseRef = do
  _ <- char '$'
  str <- parseVar <|> some digitChar
  return $ maybe (Var str) Arg (readMaybe str :: Maybe Int)

singleQuote :: Parser String
singleQuote = char '\'' *> takeWhileP Nothing (liftA2 (&&) (liftA2 (||) isPrint isSpace) (/='\'')) <* char '\''

doubleQuote :: Parser Quote
doubleQuote = char '\"' *> many (Str <$> nonEmpty allSpaces <|> Ref <$> parseRef <|> Str <$> nonEmpty word) <* char '\"'

parseEnd :: Parser Command
parseEnd = do
  eof
  return $ Exit Nothing

parseVar :: Parser String
parseVar = liftA2 (:) (letterChar <|> char '_') (many (letterChar <|> char '_' <|> digitChar))
