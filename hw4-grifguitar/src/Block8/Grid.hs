{-# LANGUAGE InstanceSigs #-}

module Block8.Grid
  ( Grid(..)
  , up
  , down
  , left
  , right
  , gridRead
  , gridWrite
  , horizontal
  , vertical
  , neighbours
  , getInitGrid
  , getRandGrid
  , getCropUnpackGrid
  , gridPrint
  , gridPrintStr
  ) where

import Control.Monad
import Control.Comonad

import System.Random

import Block8.ListZipper

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

up :: Grid a -> Grid a
up (Grid g) = Grid (listLeft g)

down :: Grid a -> Grid a
down (Grid g) = Grid (listRight g)

left :: Grid a -> Grid a
left  (Grid g) = Grid (fmap listLeft g)

right :: Grid a -> Grid a
right (Grid g) = Grid (fmap listRight g)

gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine

horizontal :: Grid a -> ListZipper (Grid a)
horizontal = genericMove left right

vertical :: Grid a -> ListZipper (Grid a)
vertical = genericMove up down

neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where horizontals = [left, right]
        verticals   = [up, down]

getInitGrid :: a -> Grid a
getInitGrid x = Grid $ getInitLZ (getInitLZ x)

getRandGrid :: Int -> Grid Int
getRandGrid x = Grid $ genericMove (fmap (* 3)) (fmap (* 5)) (getRandLZ x)

getCropUnpackGrid :: Int -> Grid a -> [[a]]
getCropUnpackGrid size (Grid g) = getCropUnpackLZ size res
  where
    res = getCropUnpackLZ size <$> g

gridPrint :: Int -> (a -> Char) -> Grid a -> IO ()
gridPrint size f g1 = mapM_ putStrLn res
  where
    g2 = getCropUnpackGrid size g1
    res = fmap (fmap f) g2

gridPrintStr :: Int -> (a -> String) -> Grid a -> IO ()
gridPrintStr size f g1 = mapM_ (putStrLn . unwords) res
  where
    g2 = getCropUnpackGrid size g1
    res = fmap (fmap f) g2

instance Functor Grid where
  fmap :: (a -> b) -> Grid a -> Grid b
  fmap f (Grid g) = Grid (fmap f <$> g)

instance Comonad Grid where
    extract :: Grid a -> a
    extract = gridRead

    duplicate :: Grid a -> Grid (Grid a)
    duplicate = Grid . fmap horizontal . vertical