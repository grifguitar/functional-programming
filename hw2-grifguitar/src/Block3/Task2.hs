{-# LANGUAGE InstanceSigs #-}

module Block3.Task2
  ( Parser(..)
  , ok
  , eof
  , satisfy
  , element
  , stream
  ) where

import Block3.Task1

ok :: Parser s ()
ok = pure ()

checkEof :: [s] -> Maybe ((), [s])
checkEof [] = Just ((), [])
checkEof _ = Nothing

eof :: Parser s ()
eof = Parser (checkEof)

checkSatisfy :: (s -> Bool) -> [s] -> Maybe (s, [s])
checkSatisfy _ [] = Nothing
checkSatisfy predict (x : xs) = if (predict x)
                                then Just (x, xs)
                                else Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy predict = Parser (checkSatisfy predict)

element :: (Eq s) => s -> Parser s s
element sample = satisfy ((==) sample)

stream :: (Eq s) => [s] -> Parser s [s]
stream (sample : pattern) = (<*>) (fmap (:) (element sample)) (stream pattern)
stream [] = pure []