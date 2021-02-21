{-# LANGUAGE InstanceSigs #-}

module Block1.Task3
  ( Node(..)
  , isEmpty
  , getElementsAmount
  , findElement
  , insertElement
  , fromList
  , removeElement
  ) where

import qualified Data.List.NonEmpty as List

data Node a = ZZ | MT (List.NonEmpty a) (Node a) (Node a) deriving Show

instance (Eq a) => Eq (Node a) where
  (==) :: Node a -> Node a -> Bool
  (MT list1 nodeL1 nodeR1) == (MT list2 nodeL2 nodeR2) =
    (list1 == list2) && (nodeL1 == nodeL2) && (nodeR1 == nodeR2)
  ZZ == ZZ = True
  _ == _ = False

isEmpty :: Node a -> Bool
isEmpty MT {} = False
isEmpty _ = True

getElementsAmount :: Node a -> Int
getElementsAmount (MT list1 nodeL1 nodeR1) =
  (+) (List.length list1) $ (+) (getElementsAmount nodeL1) (getElementsAmount nodeR1)
getElementsAmount ZZ = 0

findElement :: (Ord a) => a -> Node a -> Bool
findElement x (MT list1 nodeL1 nodeR1)
  | x < val = findElement x nodeL1
  | x > val = findElement x nodeR1
  | otherwise = True
    where
      val = List.head list1
findElement _ ZZ = False

insertElement :: (Ord a) => a -> Node a -> Node a
insertElement x (MT list1 nodeL1 nodeR1)
  | x < val = MT list1 (insertElement x nodeL1) nodeR1
  | x > val = MT list1 nodeL1 (insertElement x nodeR1)
  | otherwise = MT (List.cons x list1) nodeL1 nodeR1
    where
      val = List.head list1
insertElement x ZZ = MT (x List.:| []) ZZ ZZ

fromList :: (Ord a) => [a] -> Node a
fromList = foldr insertElement ZZ

removeElement :: (Ord a) => a -> Node a -> Node a
removeElement x (MT list1@(val List.:| y2 : ys) nodeL1 nodeR1)
  | x < val = MT list1 (removeElement x nodeL1) nodeR1
  | x > val = MT list1 nodeL1 (removeElement x nodeR1)
  | otherwise = MT (y2 List.:| ys) nodeL1 nodeR1
removeElement x (MT list1@(val List.:| []) nodeL1 nodeR1)
  | x < val = MT list1 (removeElement x nodeL1) nodeR1
  | x > val = MT list1 nodeL1 (removeElement x nodeR1)
  | otherwise = mergeNodes nodeL1 nodeR1
    where
      mergeNodes :: Node a -> Node a -> Node a
      mergeNodes ZZ node = node
      mergeNodes (MT lst1 ndL1 ndR1) node = MT lst1 ndL1 (mergeNodes ndR1 node)
removeElement _ ZZ = ZZ

instance Foldable Node where
  foldMap :: Monoid m => (a -> m) -> Node a -> m
  foldMap f (MT list1 nodeL1 nodeR1) =
    mappend (foldMap f nodeL1) $ mappend (mconcat $ List.toList $ List.map f list1) (foldMap f nodeR1)
  foldMap _ ZZ = mempty
  
  foldr :: (a -> b -> b) -> b -> Node a -> b
  foldr f accumulator (MT list1 nodeL1 nodeR1) =
    foldr f (foldr f (foldr f accumulator nodeR1) list1) nodeL1
  foldr _ accumulator ZZ = accumulator