{-# LANGUAGE InstanceSigs #-}

module Block1.Task2
  ( Nat(..)
  ) where

data Nat = Z | S Nat deriving Show

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (S firstArg) == (S secondArg) = firstArg == secondArg
  Z == Z = True
  _ == _ = False

instance Ord Nat where
  (<=) :: Nat -> Nat -> Bool
  (S firstArg) <= (S secondArg) = firstArg <= secondArg
  Z <= _ = True
  _ <= _ = False

instance Num Nat where
  (+) :: Nat -> Nat -> Nat
  (S firstArg) + secondArg = firstArg + S secondArg
  Z + secondArg = secondArg

  (*) :: Nat -> Nat -> Nat
  (S firstArg) * secondArg = firstArg * secondArg + secondArg
  Z * _ = Z

  abs :: Nat -> Nat
  abs x = x

  signum :: Nat -> Nat
  signum (S _) = S Z
  signum Z     = S Z

  fromInteger :: Integer -> Nat
  fromInteger x
    | x == 0 = Z
    | x < 0 = error "Nat does not exist for negative Integer"
    | otherwise = S (fromInteger (x - 1))

  (-) :: Nat -> Nat -> Nat
  (S firstArg) - (S secondArg) = firstArg - secondArg
  firstArg - Z = firstArg
  Z - _ = Z

instance Enum Nat where
  succ :: Nat -> Nat
  succ x = S x

  pred :: Nat -> Nat
  pred (S x) = x
  pred Z     = error "pred does not exist for zero"

  toEnum :: Int -> Nat
  toEnum x = fromInteger (toInteger x)

  fromEnum :: Nat -> Int
  fromEnum (S x) = (+) 1 (fromEnum x)
  fromEnum Z     = 0

instance Real Nat where
  toRational :: Nat -> Rational
  toRational x = toRational (fromEnum x)

instance Integral Nat where
  toInteger :: Nat -> Integer
  toInteger x = toInteger (fromEnum x)

  quotRem :: Nat -> Nat -> (Nat, Nat)
  quotRem _ Z = error "cannot be divided by zero"
  quotRem firstArg secondArg = res
    where
      increaseFirstArgument :: (Enum a) => (a, a) -> (a, a)
      increaseFirstArgument (x, y) = (succ x, y)
      res = if firstArg >= secondArg
            then increaseFirstArgument (quotRem (firstArg - secondArg) secondArg)
            else (0, firstArg)