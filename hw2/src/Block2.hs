
module Block2
  (
    Expr(..)
  , ArithmeticError(..)
  , eval

  , moving
  ) where

import Control.Applicative (liftA2)
import Control.Monad.State (execState, get, put, replicateM_, State)


data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Const Int
    deriving (Show)

data ArithmeticError = DivizionByZero | NegativeExponent
  deriving (Show, Eq, Ord)

op :: (Int -> Int -> Int) -> Expr -> Expr -> Either ArithmeticError Int
op f x y = liftA2 f (eval x) (eval y)

eval :: Expr -> Either ArithmeticError Int
eval (Add a b) = op (+) a b
eval (Sub a b) = op (-) a b
eval (Mul a b) = op (*) a b
eval (Div a b) = let b' = eval b in
  if b' == Right 0 then Left DivizionByZero
  else liftA2 div (eval a) b'
eval (Pow a b) = let b' = eval b in
  if b' == Left DivizionByZero then b'
  else
    if b' < Right 0 then Left NegativeExponent
    else liftA2 (^) (eval a) b'
eval (Const x) = pure x


moving :: Int -> [Int] -> [Double]
moving size list =
  let (_, _, ans) = execState
                      (replicateM_ (length list) $ step size (toEnum <$> list))
                      (0, 0, [])
  in reverse ans

step :: Int -> [Double] -> State (Int, Double, [Double]) ()
step size arr = do
  (i, prev, acc) <- get
  let minus = if i - size < 0 then 0.0 else arr !! (i - size)
  let summ = prev - minus + (arr !! i)
  let new = summ / toEnum (min (i + 1) size)
  put (i + 1, summ, new : acc)
