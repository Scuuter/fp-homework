{-# LANGUAGE InstanceSigs #-}

module Block4 where

data Pair a = Pair a a

instance Foldable Pair where
  foldMap :: Monoid m => (a -> m) -> Pair a -> m
  foldMap f (Pair x y) = mappend (f x) (f y)

  foldr :: (a -> b -> b) -> b -> Pair a -> b
  foldr f z (Pair x y) = x `f` (y `f` z)

data NonEmpty a = a :| [a]

instance Foldable NonEmpty where
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  -- naming as on hackage
  foldMap f (x :| []) = f x
  foldMap f (x :| xs) = mappend (f x) (foldMap f xs)

  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f z (x :| []) = f x z
  foldr f z (x :| xs) = f x (foldr f z xs)


splitOn :: Eq a => a -> [a] -> NonEmpty [a]
--splitOn _ [] = [] :| []
splitOn separator = undefined
