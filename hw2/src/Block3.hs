{-# LANGUAGE InstanceSigs #-}

module Block3
  (
    Parser(..)
  , Alternative(..)

  , element
  , eof
  , isnot
  , ok
  , satisfy
  , stream

  , signedInt
  , psp
  ) where

import Data.Bifunctor (first)
import Data.Char (isDigit)
import Control.Applicative (Alternative(..), empty, liftA2, (<|>))
import Control.Monad ((>=>))

newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser run) = Parser $ fmap (first f) . run
-- (Maybe (a, [s]) -> Maybe (b, [s])) . ([s] -> Maybe (a, [s])) == [s] -> Maybe (b, [s])

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = Parser $ \s -> Just (a, s)

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  Parser f <*> Parser a = Parser $ f >=> \(f', s') ->
                            a s' >>= \(a', s'') ->
                              return (f' a', s'')
{- ver 2
    \s ->
      f s >>= \(f', s') ->
        a s' >>= \(a', s'') ->
          return (f' a', s'')
-}
{- ver 1
    case f s of
      Nothing       -> Nothing
      Just (f', s') -> case a s' of
        Nothing        -> Nothing
        Just (a', s'') -> Just (f' a', s'')
-}

instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  Parser run >>= f = Parser $ \s ->
    case run s of
      Nothing      -> Nothing
      Just (a, s') -> runParser (f a) s' --let Parser run' = f a in run' s'

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ const Nothing

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  Parser a <|> Parser b = Parser $ \s -> a s <|> b s

ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

eof :: Parser s ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

isnot :: Parser s a -> Parser s ()
isnot parser = Parser $ \s -> case runParser parser s of
  Just _  -> Nothing
  Nothing -> Just ((), s)

satisfy :: (s -> Bool) -> Parser s s
satisfy p = isnot eof *> (Parser $ \(x:xs) -> if p x then Just (x, xs) else Nothing)

element :: Eq s => s -> Parser s s
element el = satisfy (== el)

stream :: Eq s => [s] -> Parser s [s]
stream [] = return []
stream (x:xs) = element x >> stream xs >> return (x:xs)

signedInt :: Parser Char Int
signedInt = read <$> (liftA2 (:) (element '-') int <|> (element '+' *> int) <|> int)

int :: Parser Char String
int = some $ satisfy isDigit

psp :: Parser Char String
psp = balance 0
  where
    balance :: Int -> Parser Char String
    balance 0 = (const "" <$> eof) <|> paren 0
    balance i = paren i

    paren i = liftA2 (:) (element '(') (balance (i+1)) <|> liftA2 (:) (element ')') (balance (i - 1))
