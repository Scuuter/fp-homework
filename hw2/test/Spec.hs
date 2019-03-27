module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)
import Block1Spec (block1)
import Block2Spec (block2)
import Block3Spec (block3)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [block1, block2, block3]
