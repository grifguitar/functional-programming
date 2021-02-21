{-# LANGUAGE InstanceSigs #-}

module Block8.ListZipper
  ( ListZipper(..)
  , listLeft
  , listRight
  , listWrite
  , toList
  , iterateTail
  , genericMove
  , getInitLZ
  , getRandLZ
  , getCropUnpackLZ
  ) where

import Control.Comonad

import System.Random

data ListZipper a = LZ [a] a [a]

listLeft :: ListZipper a -> ListZipper a
listLeft (LZ (a:as) x bs) = LZ as a (x:bs)
listLeft _ = error "listLeft"

listRight :: ListZipper a -> ListZipper a
listRight (LZ as x (b:bs)) = LZ (x:as) b bs
listRight _ = error "listRight"

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

genericMove :: (z a -> z a)
            -> (z a -> z a)
            -> z a
            -> ListZipper (z a)
genericMove f g e = LZ (iterateTail f e) e (iterateTail g e)

getInitLZ :: a -> ListZipper a
getInitLZ x = LZ (repeat x) x (repeat x)

getRandLZ :: Int -> ListZipper Int
getRandLZ x = LZ (randoms (mkStdGen (1 - x))) x (randoms (mkStdGen x))

getCropUnpackLZ :: Int -> ListZipper a -> [a]
getCropUnpackLZ size (LZ as x bs) = ls ++ [x] ++ rs
  where
    ls = reverse $ take size as
    rs = take size bs

instance Functor ListZipper where
  fmap :: (a -> b) -> ListZipper a -> ListZipper b
  fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

instance Comonad ListZipper where
  extract :: ListZipper a -> a
  extract (LZ _ x _) = x

  duplicate :: ListZipper a -> ListZipper (ListZipper a)
  duplicate = genericMove listLeft listRight