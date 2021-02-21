{-# LANGUAGE ScopedTypeVariables #-}

module Block8.Comonad19
  ( Param(..)
  , Health(..)
  , Human(..)
  , oneStep
  , run
  ) where

import Control.Monad
import Control.Comonad
import System.Random
import System.Console.ANSI

import Block8.ListZipper
import Block8.Grid

data Param = Param { p :: Double
                   , incubation :: Int
                   , sickness :: Int
                   , immunity :: Int }

data Health = Healthy | Infected | Sick | Immune

data Human = Human { health :: Health
                   , period :: Int
                   , sgen :: StdGen }

getInfectedHuman :: Param -> Human
getInfectedHuman param = Human Infected (incubation param) $ mkStdGen 0

getEmptyModel :: Param -> Grid Human
getEmptyModel param = fmap (Human Healthy 0 . mkStdGen) (getRandGrid 1)

getInitModel :: Param -> Grid Human
getInitModel param = gridWrite (getInfectedHuman param) (getEmptyModel param)

isInfected :: Human -> Bool
isInfected (Human Infected _ _) = True
isInfected (Human Sick _ _) = True
isInfected _ = False

infectedCount :: [Human] -> Int
infectedCount = length . filter isInfected

infectedNeighbours :: Grid Human -> Int
infectedNeighbours g = infectedCount
                  $ map (\direction -> extract $ direction g) neighbours

tryInfectK :: Param -> Int -> Human -> Human
tryInfectK param k h1 = h2
  where
    prob = 1.0 - (1.0 - p param) ^ k
    sgen2 :: (Double, StdGen)
    sgen2 = random $ sgen h1
    rndValue = fst sgen2
    h2 = if rndValue < prob
         then Human Infected (incubation param) $ snd sgen2
         else Human (health h1) (period h1) $ snd sgen2

rule :: Param -> Grid Human -> Human
rule param g = ans
  where
    human = extract g
    x = period human
    ans = if x > 0
          then Human (health human) (x - 1) (sgen human)
          else case health human of
            Immune -> Human Healthy 0 (sgen human)
            Healthy -> tryInfectK param (infectedNeighbours g) human
            Sick -> Human Immune (immunity param) (sgen human)
            Infected -> Human Sick (sickness param) (sgen human)

evolve :: Param -> Grid Human -> Grid Human
evolve param = extend $ rule param

humanToChar :: Human -> Char
humanToChar (Human Healthy _ _) = ' '
humanToChar (Human Infected _ _) = 'x'
humanToChar (Human Sick _ _) = '#'
humanToChar (Human Immune _ _) = '@'

oneStep :: Param -> Grid Human -> IO ()
oneStep param g = do
  line <- getLine
  case line of
    "exit" -> return ()
    _ -> do
      clearScreen
      gridPrint 10 humanToChar g
      oneStep param (evolve param g)

run :: IO ()
run = do
  (p2 :: Double) <- readLn
  (incubation2 :: Int) <- readLn
  (sickness2 :: Int) <- readLn
  (immunity2 :: Int) <- readLn
  let cfg = Param p2 incubation2 sickness2 immunity2
  oneStep cfg (getInitModel cfg)