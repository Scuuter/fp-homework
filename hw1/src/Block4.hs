{-# LANGUAGE InstanceSigs #-}

module Block4
  (
    Pair(..)
  , NonEmpty((:|))
  , checkJoinWith
  , foldr
  , foldMap
  , joinWith
  , splitOn
  ) where


data Pair a = Pair a a

instance Foldable Pair where
  foldMap :: Monoid m => (a -> m) -> Pair a -> m
  foldMap f (Pair x y) = mappend (f x) (f y)

  foldr :: (a -> b -> b) -> b -> Pair a -> b
  foldr f z (Pair x y) = x `f` (y `f` z)


data NonEmpty a = a :| [a]
  deriving (Show)

instance Foldable NonEmpty where
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  -- naming as on hackage
  foldMap f (x :| []) = f x
  foldMap f (x :| xs) = mappend (f x) (foldMap f $ fromList xs)

  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f z (x :| []) = f x z
  foldr f z (x :| xs) = f x (foldr f z xs)

fromList :: [a] -> NonEmpty a
fromList [x]    = x :| []
fromList (x:xs) = x :| xs
fromList _      = error "list is empty"

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn separator = foldr ( \a (x:|xs) ->
                              if a == separator
                                then []:|(x:xs)
                                else (a:x):|xs
                          )
                    ([]:|[])

joinWith :: a -> NonEmpty [a] -> [a]
joinWith separator (x:|xs) = x ++ foldMap (separator:) xs

checkJoinWith :: Eq a => a -> [a] -> Bool
checkJoinWith x list = (joinWith x . splitOn x) list == list
