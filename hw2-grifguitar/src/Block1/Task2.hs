{-# LANGUAGE InstanceSigs #-}

module Block1.Task2
  ( Tree(..)
  ) where

data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a

instance (Eq a) => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (==) (Leaf x) (Leaf y) = x == y
  (==) (Branch l1 r1) (Branch l2 r2) = (l1 == l2) && (r1 == r2)
  (==) _ _ = False

instance (Show a) => Show (Tree a) where
  show :: Tree a -> String
  show (Leaf x) = "(" ++ show x ++ ")"
  show (Branch l r) = "(" ++ show l ++ "," ++ show r ++ ")"

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Branch leftTree rightTree) = Branch (fmap f leftTree) (fmap f rightTree)
  fmap f (Leaf node) = Leaf (f node)

instance Applicative Tree where
  pure :: a -> Tree a
  pure node = Leaf node
  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (<*>) (Branch lf rf) (Branch leftTree rightTree) = Branch ((<*>) lf leftTree) ((<*>) rf rightTree)
  (<*>) (Branch lf rf) (Leaf node) = Branch ((<*>) lf (Leaf node)) ((<*>) rf (Leaf node))
  (<*>) (Leaf f) (Branch leftTree rightTree) = Branch ((<*>) (Leaf f) leftTree) ((<*>) (Leaf f) rightTree)
  (<*>) (Leaf f) (Leaf node) = Leaf (f node)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f acc (Leaf node) = f node acc
  foldr f acc (Branch leftTree rightTree) = foldr f (foldr f acc rightTree) leftTree

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse wrap (Branch leftTree rightTree) = 
    (<*>) (fmap Branch (traverse wrap leftTree)) (traverse wrap rightTree)
  traverse wrap (Leaf node) =
    fmap Leaf (wrap node)