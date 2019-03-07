{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}

module Block5
  (
    maybeConcat

  , Block4.NonEmpty((:|))
  , ThisOrThat(..)
  , (<>)
  , mempty
  , mappend

  , Endo(..)
  , Name(..)

  , Builder(..)
  , fromString
  , toString
  ) where

import Data.Maybe (fromMaybe)
import Block4 (NonEmpty((:|)))

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = foldr (\x xs -> fromMaybe [] x ++ xs ) []


instance Semigroup (NonEmpty a) where
  (x:|xs) <> (y:|ys) = x :| (xs ++ y:ys)


data ThisOrThat a b = This a | That b | Both a b

instance Semigroup (ThisOrThat a b) where
  This a   <> This _   = This a
  That _   <> That b   = That b
  This a   <> That b   = Both a b
  That a   <> This b   = Both b a
  This c   <> Both _ b = Both c b
  That _   <> Both a b = Both a b
  Both a b <> This _   = Both a b
  Both a _ <> That c   = Both a c
  Both a b <> Both _ _ = Both a b


newtype Name = Name String
  deriving (Show)

instance Semigroup Name where
  (Name a)  <> (Name "") = Name a
  (Name "") <> (Name a)  = Name a
  (Name a)  <> (Name b)  = Name (a ++ "." ++ b)

instance Monoid Name where
  mempty = Name ""

-- The monoid of endomorphisms under composition. (c) hackage
newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  (Endo a) <> (Endo b) = Endo (a . b)

instance Monoid a => Monoid (Endo a) where
  mempty = Endo id


data Builder = One Char | Many [Builder]
  deriving (Show)

instance Semigroup Builder where
  One a  <> One b  = Many [One a, One b]
  One a  <> Many b = Many (One a:b)
  Many a <> One b  = Many (a ++ [One b])
  Many a <> Many b = Many (a ++ b)

instance Monoid Builder where
  mempty = Many []

fromString :: String -> Builder
fromString = foldMap One

toString :: Builder -> String
toString (One a) = [a]
toString (Many a) = foldr (\x y -> toString x ++ y) [] a
