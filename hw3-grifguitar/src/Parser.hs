{-# LANGUAGE InstanceSigs #-}

module Parser
  ( Parser(..)
  , operationsParser
  ) where

import Control.Applicative
import Data.Functor

import Operations

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

checkSatisfy :: (s -> Bool) -> [s] -> Maybe (s, [s])
checkSatisfy _ [] = Nothing
checkSatisfy predict (x : xs) = if (predict x)
                                then Just (x, xs)
                                else Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy predict = Parser (checkSatisfy predict)

element :: (Eq s) => s -> Parser s s
element sample = satisfy ((==) sample)

notElement :: (Eq s) => s -> Parser s s
notElement sample = satisfy ((/=) sample)

anyChar :: Parser Char Char
anyChar = satisfy (\_ -> True)

stream :: (Eq s) => [s] -> Parser s [s]
stream (sample : pattern) = (<*>) (fmap (:) (element sample)) (stream pattern)
stream [] = pure []

spacesParser :: Parser Char String
spacesParser = many (element ' ')

pathParser :: Parser Char FilePath
pathParser = many (notElement ' ')

stringParser :: Parser Char String
stringParser = many anyChar

operationsParser :: Parser Char Operations
operationsParser =
  (<*>) (stream "cd" $> Cd <* spacesParser) pathParser <|>
  (stream "dir" $> Dir) <|>
  (<*>) (stream "ls" $> Ls <* spacesParser) pathParser <|>
  (<*>) (stream "create-folder" $> CreateFolder <* spacesParser) pathParser <|>
  (<*>) (stream "cat" $> Cat <* spacesParser) pathParser <|>
  (<*>) (stream "create-file" $> CreateFile <* spacesParser) pathParser <|>
  (<*>) (stream "remove" $> Remove <* spacesParser) pathParser <|>
  (<*>) ((<*>) (stream "write-file" $> WriteFile <* spacesParser) pathParser <* spacesParser) stringParser <|>
  (<*>) (stream "find-file" $> FindFile <* spacesParser) pathParser <|>
  (<*>) (stream "information" $> Inform <* spacesParser) pathParser <|>
  (stream "help" $> Help) <|>
  (stream "exit" $> Exit)