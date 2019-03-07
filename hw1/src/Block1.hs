module Block1
  (
    contains
  , smartReplicate
  , order3
  , stringSum
  ) where

import Data.List (sort)

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (a, b, c) = let list = sort [a, b, c] in (head list, list !! 1, last list)

smartReplicate :: [Int] -> [Int]
smartReplicate = foldr (\ x -> (++) (replicate x x)) []

contains :: Eq a => a -> [[a]] -> [[a]]
contains x = filter (elem x)

stringSum :: String -> Int
stringSum s = sum $ map read $ words s
