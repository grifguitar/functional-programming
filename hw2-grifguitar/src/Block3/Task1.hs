{-# LANGUAGE InstanceSigs #-}

module Block3.Task1
  ( Parser(..)
  ) where

import Control.Applicative (Alternative(..))

data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

applyFirst :: (a -> b) -> (a, s) -> (b, s)
applyFirst f (x, str) = (f x, str)

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser parser) = Parser (fmap (applyFirst f) . parser)

applicativeParser :: Parser s (a -> b) -> Parser s a -> [s] -> Maybe (b, [s])
applicativeParser fParser xParser str = do
  (f, str1) <- runParser fParser str
  (x, str2) <- runParser xParser str1
  return (f x, str2)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure x = Parser (\str -> Just (x, str))
  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) fParser xParser = Parser (applicativeParser fParser xParser)

bindParser :: Parser s a -> (a -> Parser s b) -> [s] -> Maybe (b, [s])
bindParser xParser f str = do
  (x, str1) <- runParser xParser str
  runParser (f x) str1

instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (>>=) xParser f = Parser (bindParser xParser f)

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser (\_ -> Nothing)
  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (<|>) firstParser secondParser = Parser (\str ->
    (<|>) (runParser firstParser str) (runParser secondParser str))