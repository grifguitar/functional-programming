{-# LANGUAGE InstanceSigs #-}

module Block2.Task1
  ( Expr(..)
  , ArithmeticError(..)
  , eval
  ) where

data ArithmeticError = DivisionByZero
                     | NegativeExponentiation

data Expr = Const Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr

instance Eq Expr where
  (==) :: Expr -> Expr -> Bool
  (==) (Const x) (Const y) = x == y
  (==) (Add l1 r1) (Add l2 r2) = (l1 == l2) && (r1 == r2)
  (==) (Sub l1 r1) (Sub l2 r2) = (l1 == l2) && (r1 == r2)
  (==) (Mul l1 r1) (Mul l2 r2) = (l1 == l2) && (r1 == r2)
  (==) (Div l1 r1) (Div l2 r2) = (l1 == l2) && (r1 == r2)
  (==) (Pow l1 r1) (Pow l2 r2) = (l1 == l2) && (r1 == r2)
  (==) _ _ = False

instance Show Expr where
  show :: Expr -> String
  show (Const x) = show x
  show (Add l r) = "(" ++ show l ++ " + " ++ show r ++ ")"
  show (Sub l r) = "(" ++ show l ++ " - " ++ show r ++ ")"
  show (Mul l r) = "(" ++ show l ++ " * " ++ show r ++ ")"
  show (Div l r) = "(" ++ show l ++ " / " ++ show r ++ ")"
  show (Pow l r) = "(" ++ show l ++ " ^ " ++ show r ++ ")"

instance Eq ArithmeticError where
  (==) :: ArithmeticError -> ArithmeticError -> Bool
  (==) DivisionByZero DivisionByZero = True
  (==) NegativeExponentiation NegativeExponentiation = True
  (==) _ _ = False

instance Show ArithmeticError where
  show :: ArithmeticError -> String
  show DivisionByZero = "Division by zero"
  show NegativeExponentiation = "Negative exponentiation"

eval :: Expr -> Either ArithmeticError Int
eval (Const x) = Right x
eval (Add l r) = abstractEval l r (+)
eval (Sub l r) = abstractEval l r (-)
eval (Mul l r) = abstractEval l r (*)

eval (Div l r) = do
  y <- eval r
  if y /= 0
  then do
    x <- eval l
    Right (div x y)
  else
    Left DivisionByZero

eval (Pow l r) = do
  y <- eval r
  if y >= 0
  then do
    x <- eval l
    Right (x ^ y)
  else
    Left NegativeExponentiation

abstractEval :: Expr -> Expr -> (Int -> Int -> Int) -> Either ArithmeticError Int
abstractEval l r f = (<*>) (fmap f (eval l)) (eval r)