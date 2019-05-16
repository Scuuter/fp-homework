module Main where

import Tasks (
    multiply
  , slowMultiply
  , perimeter
  , doubleArea

  , Point(..)
  )

import Criterion.Main (
    bgroup
  , bench
  , defaultMain
  , whnf
  )

kek :: Int -> [[Int]]
kek x = replicate x (replicate x 1488)

lol :: Int -> [Point]
lol x = down x ++ right 1 x ++ up (x - 1) 1


down :: Int -> [Point]
down 0 = [Point 0 0]
down x = Point 0 x : down (x - 1)

right :: Int -> Int -> [Point]
right i 0 = [Point i 0]
right i x = Point i 0 : right (i + 1) (x - 1)

up :: Int -> Int -> [Point]
up 0 y = [Point 0 y]
up x y = Point x y : up (x - 1) (y + 1)

main :: IO ()
main = defaultMain [
    bgroup "multiply" [
        bench "parallel multiply 1" $ whnf (multiply $ kek 1000) $ kek 1000
      , bench "naive multiply 1" $ whnf (slowMultiply $ kek 1000) $ kek 1000
      , bench "parallel multiply 2" $ whnf (multiply $ kek 2000) $ kek 2000
      , bench "naive multiply 2" $ whnf (slowMultiply $ kek 2000) $ kek 2000
    ]
  , bgroup "perimeter" [
        bench "1" $ whnf perimeter $ lol 1000
      , bench "2" $ whnf perimeter $ lol 10000
    ]
  , bgroup "doubleArea" [
        bench "1" $ whnf doubleArea $ lol 1000
      , bench "2" $ whnf doubleArea $ lol 10000
    ]
  ]
