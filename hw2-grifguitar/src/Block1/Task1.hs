module Block1.Task1
  ( stringSum
  ) where

import Text.Read (readMaybe)

getMaybeSum :: Maybe [Int] -> Maybe Int
getMaybeSum = fmap sum

toMaybeListOfInt :: String -> Maybe [Int]
toMaybeListOfInt s = traverse readMaybe (words s)

stringSum :: String -> Maybe Int
stringSum s = getMaybeSum (toMaybeListOfInt s)