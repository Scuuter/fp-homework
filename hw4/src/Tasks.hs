{-# LANGUAGE InstanceSigs #-}

module Tasks (
    multiply
  , slowMultiply
  , Point(..)
  , plus
  , minus
  , crossProduct
  , scalarProduct
  , perimeter
  , doubleArea
) where

import Data.List (transpose, foldl')
import Control.Parallel.Strategies (runEval, rpar, rseq)

-- task 1

multiply :: Num a => [[a]] -> [[a]] -> Maybe [[a]]
multiply a b =
  let b' = transpose b in
    if length a /= length b'
      then Nothing
      else Just $ mult' a b'

mult' :: Num a => [[a]] -> [[a]] -> [[a]]
mult' [] _ = []
mult' (a:as) b = runEval $ do
  x <- rpar $ row a b
  xs <- rseq $ mult' as b
  return (x:xs)

row :: Num a => [a] -> [[a]] -> [a]
row _ [] = []
row a (b:bs) = runEval $ do
  x <- rpar $ cell a b
  xs <- rseq $ row a bs
  return (x:xs)

slowMultiply :: Num a => [[a]] -> [[a]] -> Maybe [[a]]
slowMultiply a b =
  let b' = transpose b in
    if length a /= length b'
      then Nothing
      else Just $ slowMult' a b'

slowMult' :: Num a => [[a]] -> [[a]] -> [[a]]
slowMult' [] _ = []
slowMult' (a:as) b = slowRow a b : slowMult' as b

slowRow :: Num a => [a] -> [[a]] -> [a]
slowRow _ [] = []
slowRow a (b:bs) = cell a b : slowRow a bs

cell :: Num a => [a] -> [a] -> a
cell a b = sum $ zipWith (*) a b

-- task2

data Point = Point Int Int
  deriving (Show)

plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

crossProduct :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = x1*y2 - y1*x2

scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = x1*x2 + y1*y2

perimeter :: [Point] -> Double
perimeter (x:xs) =
  let (last', per) = coolFold dist (x, 0.0) xs in
    per + dist last' x
perimeter _ = error "Not enough points"

doubleArea :: [Point] -> Int
doubleArea (x:xs) =
  if length xs <= 1
    then error "Not enough points"
    else let (last', sqr) = coolFold crossProduct (x, 0) xs in
      sqr + crossProduct last' x
doubleArea _ = error "Not enough points"

coolFold :: (Foldable t, Num a) =>
                  (b -> b -> a) -> (b, a) -> t b -> (b, a)
coolFold fun = foldl' (\(prev, acc) point -> (point, fun prev point + acc))

length' :: Point -> Double
length' a = sqrt $ toEnum $ scalarProduct a a

dist :: Point -> Point -> Double
dist a b = length' $ minus a b
