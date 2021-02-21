module Block2.Task2
  ( moving
  ) where

import Control.Monad.State

average :: (Fractional a) => Int -> a -> State [a] a
average n x = do
  elements <- get
  let nLastElements = take n (x : elements)
  let result = sum nLastElements / fromIntegral (length nLastElements)
  put nLastElements
  return result

getCalcState :: (Fractional a) => Int -> [a] -> State [a] [a]
getCalcState n = mapM (average n)

moving :: (Fractional a) => Int -> [a] -> [a]
moving n xs = if n > 0
              then evalState (getCalcState n xs) []
              else error "the quantity must be positive"