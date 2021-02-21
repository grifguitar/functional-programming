{-# LANGUAGE InstanceSigs #-}

module Block3.Task3
  ( Parser(..)
  , CBSequence
  , cbsParser
  , intParser
  ) where

import Block3.Task2
import Control.Applicative
import Data.Char

-- Correct bracket sequence: ( CBSequence ) CBSequence
data CBSequence = Union CBSequence CBSequence
                | None

onlyCBSParser :: Parser Char CBSequence
onlyCBSParser = (<|>) (pure None) ((<*>) (fmap Union parser) onlyCBSParser)
  where
    parser = (element '(') *> onlyCBSParser <* (element ')')

cbsParser :: Parser Char CBSequence
cbsParser = (<*) onlyCBSParser eof

unite :: (Num a) => a -> a -> a
unite x y = x * 10 + y

convertToDecimal :: (Num a) => [a] -> a
convertToDecimal = foldl1 unite

onlyDigitsParser :: Parser Char Int
onlyDigitsParser = fmap convertToDecimal (some (fmap digitToInt (satisfy isDigit)))

negativeParser :: Parser Char (Int -> Int)
negativeParser = fmap (const negate) (element '-')

positiveParser :: Parser Char (Int -> Int)
positiveParser = fmap (const id) ((element '+') <|> (pure '+'))

intParser :: Parser Char Int
intParser = (<*>) ((<|>) negativeParser positiveParser) onlyDigitsParser