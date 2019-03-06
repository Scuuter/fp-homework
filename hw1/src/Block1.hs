module Block1 where

import Data.List (sort)

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (a, b, c) = let list = sort [a, b, c] in (head list, list !! 1, last list)

smartReplicate :: [Int] -> [Int]
--smartReplicate [] = []
--smartReplicate (x:xs) = replicate x x ++ (smartReplicate xs)
smartReplicate = foldr (\ x -> (++) (replicate x x)) []

contains :: Eq a => a -> [[a]] -> [[a]]
contains x = filter (elem x)

stringSum :: String -> Int
stringSum s = sum $ map read $ words s
