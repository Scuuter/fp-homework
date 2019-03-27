{-# LANGUAGE InstanceSigs #-}

module Block1 (
    stringSum

  , Tree(..)

  , NonEmpty(..)
  ) where

import Control.Applicative (liftA2)
import Data.Foldable (toList)
import Text.Read (readMaybe)


stringSum :: String -> Maybe Int
stringSum s = fmap sum $ traverse (\x -> readMaybe x :: Maybe Int) $ words s


data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving (Show, Eq)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)
  fmap f (Leaf x) = Leaf (f x)

instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (Branch l r) <*> a = Branch (l <*> a) (r <*> a)
  Leaf f <*> a = f <$> a

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Branch l r) = mappend (foldMap f l) (foldMap f r)
  foldMap f (Leaf x) = f x

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g (Branch l r) = liftA2 Branch (traverse g l) (traverse g r) --Branch <$> traverse g l <*> traverse g r
  traverse g (Leaf x) = Leaf <$> g x


data NonEmpty a = a :| [a]
  deriving (Show, Eq)

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x:|xs) = f x :| fmap f xs

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure x = x :| []

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (f:|fs) <*> (x:|xs) = f x :| ((f <$> xs) ++ (fs <*> (x:xs)))

instance Foldable NonEmpty where
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (x:|xs) = mappend (f x) (foldMap f xs)

instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse g (x:|xs) = liftA2 (:|) (g x) (traverse g xs)

instance Monad NonEmpty where
  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (x:|xs) >>= f = let (y:|ys) = f x in
    y :| (ys ++ (xs >>= toList . f)) -- ys ++ fold (toList . f <$> xs)
