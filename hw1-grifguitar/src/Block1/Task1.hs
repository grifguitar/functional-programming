{-# LANGUAGE InstanceSigs #-}

module Block1.Task1
  ( Days(..)
  , nextDay
  , afterDays
  , isWeekend
  , daysToParty
  ) where

import Numeric.Natural (Natural)

data Days = Monday
          | Tuesday
          | Wednesday
          | Thursday
          | Friday
          | Saturday
          | Sunday
          deriving Show

instance Eq Days where
  (==) :: Days -> Days -> Bool
  firstArg == secondArg = getNumberByDay firstArg == getNumberByDay secondArg

getNumberByDay :: Days -> Natural
getNumberByDay day = case day of
  Monday    -> 0
  Tuesday   -> 1
  Wednesday -> 2
  Thursday  -> 3
  Friday    -> 4
  Saturday  -> 5
  Sunday    -> 6

getDayByNumber :: Natural -> Days
getDayByNumber n = case mod n 7 of
  0 -> Monday
  1 -> Tuesday
  2 -> Wednesday
  3 -> Thursday
  4 -> Friday
  5 -> Saturday
  6 -> Sunday
  _ -> undefined

nextDay :: Days -> Days
nextDay day = getDayByNumber $ (+) 1 $ getNumberByDay day

afterDays :: Days -> Natural -> Days
afterDays day n = getDayByNumber $ (+) n $ getNumberByDay day

isWeekend :: Days -> Bool
isWeekend day = case day of
  Saturday -> True
  Sunday   -> True
  _        -> False

daysToParty :: Days -> Natural
daysToParty day = mod (11 - getNumberByDay day) 7
