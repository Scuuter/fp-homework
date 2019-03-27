module Block2Spec (
  block2
) where


import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Block2 (
    Expr(..)
  , ArithmeticError(..)
  , eval
  )

block2 :: TestTree
block2 = testGroup "Block2" [evalTest]

evalTest :: TestTree
evalTest = testGroup "eval test"
  [
    testCase "Add 1" $
      eval (create Add 5 4)       @?= Right 9

  , testCase "Add 2" $
      eval (create Add 5 (-5))    @?= Right 0

  , testCase "Sub 1" $
      eval (create Sub 1488 1337) @?= Right 151

  , testCase "Sub 2" $
      eval (create Sub 30 239)    @?= Right (-209)

  , testCase "Mul 1" $
      eval (create Mul (-9) (-9)) @?= Right 81

  , testCase "Mul 2" $
      eval (create Mul (-9876) 0) @?= Right 0

  , testCase "Mul 3" $
      eval (create Mul 1 1234)    @?= Right 1234

  , testCase "Div 1" $
      eval (create Div 10 3)      @?= Right 3

  , testCase "Div 2" $
      eval (create Div 4 (-10))   @?= Right (-1)

  , testCase "Div 3" $
      eval (create Div 1336 1337) @?= Right 0

  , testCase "Pow 1" $
      eval (create Pow 2 10)      @?= Right 1024

  , testCase "Pow 2" $
      eval (create Pow 1488 1)    @?= Right 1488

  , testCase "Pow 3" $
      eval (create Pow 57 0)      @?= Right 1

  , testCase "Complex 1" $
      eval (Add (create Mul 2 2) (Const 2))
        @?= Right 6

  , testCase "Complex 2" $
      eval (Mul (create Add (-2) (-2)) (Const 2))
        @?= Right (-8)

  , testCase "DivizionByZero 1" $
      eval (create Div 1234 0)
        @?= Left DivizionByZero

  , testCase "DivizionByZero 2" $
      eval (Pow (Const 1) (Div (Const 7) (create Sub 9 9)))
        @?= Left DivizionByZero

  , testCase "NegativeExponent 1" $
      eval (create Pow 99 (-99))
        @?= Left NegativeExponent

  , testCase "NegativeExponent 2" $
      eval (Sub (Pow (Const 228) (create Mul (-4) 8)) (create Add 5 5))
        @?= Left NegativeExponent

  ]

create :: (Expr -> Expr -> Expr) -> Int -> Int -> Expr
create f a b = f (Const a) (Const b)
