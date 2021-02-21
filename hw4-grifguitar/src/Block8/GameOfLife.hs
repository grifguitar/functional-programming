module Block8.GameOfLife
  ( evolve
  , boolToChar
  ) where

import Control.Monad
import Control.Comonad

import Block8.ListZipper
import Block8.Grid

aliveCount :: [Bool] -> Int
aliveCount = length . filter id

aliveNeighbours :: Grid Bool -> Int
aliveNeighbours g = aliveCount
                  $ map (\direction -> extract $ direction g) neighbours

rule :: Grid Bool -> Bool
rule g = case aliveNeighbours g of
     2 -> extract g
     3 -> True
     _ -> False

evolve :: Grid Bool -> Grid Bool
evolve = extend rule

boolToChar :: Bool -> Char
boolToChar False = '.'
boolToChar True = 'x'