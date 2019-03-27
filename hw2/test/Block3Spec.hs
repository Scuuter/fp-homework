
module Block3Spec (
  block3
) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
--import Test.Tasty.QuickCheck (testProperty)
import Data.Char (isUpper)

import Block3 (
    Parser(..)
--  , Alternative(..)

  , element
  , eof
  , ok
  , satisfy
  , stream

  , signedInt
  , psp
  )

block3 :: TestTree
block3 = testGroup "Block3" [parsersTest]

parsersTest :: TestTree
parsersTest = testGroup "Test parsers and combinators"
  [
    testCase "ok 1" $
      runParser ok "kek" @?= Just ((), "kek")

  , testCase "ok 2" $
      runParser ok ([1,2,3,4] :: [Int]) @?= Just ((), [1,2,3,4])

  , testCase "eof Nothing" $
      runParser eof ([1,2,3,4] :: [Int]) @?= Nothing

  , testCase "eof 1" $
      runParser eof ([] :: [Int]) @?= Just ((), [])

  , testCase "eof 2" $
      runParser eof "" @?= Just ((), [])

  , testCase "satisfy Nothing [Int]" $
      runParser (satisfy (>3)) ([1,2,3,4] :: [Int]) @?= Nothing

  , testCase "satisfy Nothing []" $
      runParser (satisfy (>3)) ([] :: [Int]) @?= Nothing

  , testCase "satisfy (>3)" $
      runParser (satisfy (>3)) ([4, 2] :: [Int]) @?= Just (4, [2])

  , testCase "satisfy isUpper" $
      runParser (satisfy isUpper) "Freak" @?= Just ('F', "reak")

  , testCase "element Nothing" $
      runParser (element 'a') "" @?= Nothing

  , testCase "element a on 'bbb'" $
      runParser (element 'a') "bbb" @?= Nothing

  , testCase "element F on 'Freak'" $
      runParser (element 'F') "Freak" @?= Just ('F', "reak")

  , testCase "stream 'Freak' on 'Freak you'" $
      runParser (stream "Freak") "Freak you" @?= Just ("Freak", " you")

  , testCase "stream 'reak' on 'Freak you'" $
      runParser (stream "reak") "Freak you" @?= Nothing

  , testCase "signedInt '+1234'" $
      runParser signedInt "+1234" @?= Just (1234, "")

  , testCase "signedInt '1234 a' == Just (1234. ' a')" $
      runParser signedInt "1234 a" @?= Just (1234, " a")

  , testCase "signedInt '-1234'" $
      runParser signedInt "-1234" @?= Just (-1234, "")

  , testCase "signedInt 'nope'" $
      runParser signedInt "nope" @?= Nothing

  , testCase "signedInt ''" $
      runParser signedInt "" @?= Nothing

  , testCase "psp '(())()'" $
      runParser psp "(())()" @?= Just ("(())()", "")

  , testCase "psp '(()))' == Nothing" $
      runParser psp "(()))" @?= Nothing

  , testCase "psp '(())(()' == Nothing" $
      runParser psp "(())(()" @?= Nothing

  , testCase "psp '(())() a' == Nothing" $
      runParser psp "(())() a" @?= Nothing


  ]
