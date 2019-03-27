{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Block1Spec (
  block1
) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import Control.Monad (liftM, liftM2)
import Test.QuickCheck (Arbitrary(..), sized, oneof)

import Block1 (
    stringSum

    , Tree(..)
    , NonEmpty(..)
  )

block1 :: TestTree
block1 = testGroup "Block1" [stringSumTest, treeTest, nonEmptyTest]

stringSumTest :: TestTree
stringSumTest = testGroup "stringSum test"
  [
    testCase "Sum of integer 1" $
      stringSum "1  2 3 " @?= Just 6

  , testCase "Sum of integer 2" $
      stringSum "1 -6 3"  @?= Just (-2)

  , testCase "Sum of integer 3" $
      stringSum "0 0 0"   @?= Just 0

  , testCase "Sum of integer 4" $
      stringSum "1234"    @?= Just 1234

  , testCase "Empty string" $
      stringSum ""        @?= Just 0

  , testCase "Invalid input 1" $
      stringSum "1 a 3"   @?= Nothing

  , testCase "Invalid input 2" $
      stringSum "1 2 3.4" @?= Nothing
  ]

treeTest :: TestTree
treeTest = testGroup "Tree typeclasses laws test"
  [
    testProperty "fmap id == id (Tree Int)" $
      \tree -> fmap id (tree :: Tree Int) == id tree

  , testProperty "fmap id == id (Tree String)" $
      \tree -> fmap id (tree :: Tree String) == id tree

  , testProperty "fmap (f . g) == fmap f . fmap g (Tree Int)" $
      \tree -> fmap ((*2) . (+2)) (tree :: Tree Int) == (fmap (*2) . fmap (+2)) tree

  , testProperty "fmap (f . g) == fmap f . fmap g (Tree String)" $
      \tree -> fmap (reverse . ('x':)) (tree :: Tree String) == (fmap reverse . fmap ('x':)) tree

  , testProperty "pure id <*> v == v (Tree Int)" $
      \tree -> (pure id <*> (tree :: Tree Int)) == tree

  , testProperty "pure id <*> v == v (Tree String)" $
      \tree -> (pure id <*> (tree :: Tree String)) == tree

  , testProperty "pure (.) <*> u <*> v <*> w = u <*> (v <*> w) (Tree Int)" $
      \u v w ->
        (pure (.) <*> (u :: Tree (Int -> Int)) <*> (v :: Tree (Int -> Int)) <*> (w :: Tree Int)) == (u <*> (v <*> w))

  , testProperty "pure (.) <*> u <*> v <*> w = u <*> (v <*> w) (Tree String)" $
      \u v w ->
        (pure (.) <*> (u :: Tree (String -> String)) <*> (v :: Tree (String -> String)) <*> (w :: Tree String)) == (u <*> (v <*> w))

  , testProperty "pure f <*> pure x = pure (f x) (Tree Int)" $
      \f x ->
        ((pure f :: Tree (Int -> Int)) <*> (pure x :: Tree Int)) == pure (f x)

  , testProperty "pure f <*> pure x = pure (f x) (Tree String)" $
      \f x ->
        ((pure f :: Tree (String -> String))  <*> (pure x :: Tree String)) == pure (f x)

  ]

nonEmptyTest :: TestTree
nonEmptyTest = testGroup "NonEmpty typeclasses laws test"
  [
    testProperty "fmap id == id (NonEmpty Int)" $
      \list -> fmap id (list :: NonEmpty Int) == id list

  , testProperty "fmap id == id (NonEmpty String)" $
      \list -> fmap id (list :: NonEmpty String) == id list

  , testProperty "fmap (f . g) == fmap f . fmap g (NonEmpty Int)" $
      \list -> fmap ((*2) . (+2)) (list :: NonEmpty Int) == (fmap (*2) . fmap (+2)) list

  , testProperty "fmap (f . g) == fmap f . fmap g (NonEmpty String)" $
      \list -> fmap (reverse . ('x':)) (list :: NonEmpty String) == (fmap reverse . fmap ('x':)) list

  , testProperty "pure id <*> v == v (NonEmpty Int)" $
      \list -> (pure id <*> (list :: NonEmpty Int)) == list

  , testProperty "pure id <*> v == v (NonEmpty String)" $
      \list -> (pure id <*> (list :: NonEmpty String)) == list

  , testProperty "pure (.) <*> u <*> v <*> w = u <*> (v <*> w) (NonEmpty Int)" $
      \u v w ->
        (pure (.) <*> (u :: NonEmpty (Int -> Int)) <*> (v :: NonEmpty (Int -> Int)) <*> (w :: NonEmpty Int)) == (u <*> (v <*> w))

-- too slow, uncomment on your risk

  -- , testProperty "pure (.) <*> u <*> v <*> w = u <*> (v <*> w) (NonEmpty String)" $
  --     \u v w ->
  --       (pure (.) <*> (u :: NonEmpty (String -> String)) <*> (v :: NonEmpty (String -> String)) <*> (w :: NonEmpty String)) == (u <*> (v <*> w))

  , testProperty "pure f <*> pure x = pure (f x) (NonEmpty Int)" $
      \f x ->
        ((pure f :: NonEmpty (Int -> Int)) <*> (pure x :: NonEmpty Int)) == pure (f x)

  , testProperty "pure f <*> pure x = pure (f x) (NonEmpty String)" $
      \f x ->
        ((pure f :: NonEmpty (String -> String))  <*> (pure x :: NonEmpty String)) == pure (f x)

  ]

-- for property tests
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized tree where
    tree n | n == 0 = liftM Leaf arbitrary
           | n > 0  = oneof [liftM Leaf arbitrary,
                             liftM2 Branch tree' tree']
           | otherwise = error "Negative size"
      where tree' = tree (n `div` 2)

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = liftM2 (:|) arbitrary arbitrary

-- for Applicative rules testing
instance Show (Int -> Int) where
  show _ = show "function"

instance Show (String -> String) where
  show _ = show "function"
