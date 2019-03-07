module Block2
  (
    randomIntList
  , delete
  , mergeSort
  ) where
import System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen


-- deletes k-th element of list and returns (element, list)
-- if list contains less than k elements, returns (Nothing, list)
-- list must be finite
delete :: Int -> [a] -> (Maybe a, [a])
delete = helper []
  where
    helper saved i (x:xs) | i == 0    = (Just x, saved ++ xs)
                          | i > 0     = helper (saved ++ [x]) (i - 1) xs
                          | otherwise = (Nothing, saved ++ [x] ++ xs)
    helper saved _ [] = (Nothing, saved)


-- sorts list using merge sort algorithm
-- elements are arranged from from lowest to highest
-- list must be finite
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort list = merge (mergeSort (take mid list)) (mergeSort (drop mid list))
  where
    mid = length list `div` 2

    merge :: Ord a => [a] -> [a] -> [a]
    merge [] a                      = a
    merge a []                      = a
    merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                        | otherwise = y : merge (x:xs) ys
