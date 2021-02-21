{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module Block1.EffectiveGeometry
  ( Point(..)
  , plus
  , minus
  , scalarProduct
  , crossProduct
  , perimeter
  , doubleArea
  ) where

import GHC.Generics
import Control.DeepSeq

data Point = Point Int Int deriving (Eq, Show, NFData, Generic)

type Op = Int -> Int -> Int

abstractPointsOperation :: Op -> Point -> Point -> Point
abstractPointsOperation op (Point x1 y1) (Point x2 y2) = ($!!) id $ Point (op x1 x2) (op y1 y2)

pointToInt :: Op -> Point -> Int
pointToInt op (Point x y) = ($!!) id $ op x y

reversePoint :: Point -> Point
reversePoint (Point x y) = Point y x

plus :: Point -> Point -> Point
plus = abstractPointsOperation (+)

minus :: Point -> Point -> Point
minus = abstractPointsOperation (-)

scalarProduct :: Point -> Point -> Int
scalarProduct p1 p2 = pointToInt (+) (abstractPointsOperation (*) p1 p2)

crossProduct :: Point -> Point -> Int
crossProduct p1 p2 = pointToInt (-) (abstractPointsOperation (*) p1 (reversePoint p2))

vectorLength :: Point -> Point -> Double
vectorLength p1 p2 = ($!!) id $ sqrt $ fromIntegral $ scalarProduct p p
  where
    p :: Point
    p = minus p1 p2

perimeterUtility :: Double -> Point -> [Point] -> Double
perimeterUtility !acc p0 (p1:p2:ps) = perimeterUtility (acc + vectorLength p1 p2) p0 (p2:ps)
perimeterUtility !acc p0 (p1:_) = acc + vectorLength p1 p0
perimeterUtility !acc _ _ = acc

doubleAreaUtility :: Int -> Point -> [Point] -> Int
doubleAreaUtility !acc p0 (p1:p2:ps) = doubleAreaUtility (acc + crossProduct p1 p2) p0 (p2:ps)
doubleAreaUtility !acc p0 (p1:_) = acc + crossProduct p1 p0
doubleAreaUtility !acc _ _ = acc

perimeter :: [Point] -> Double
perimeter (p0:ps) = perimeterUtility 0 p0 (p0:ps)
perimeter _ = 0

doubleArea :: [Point] -> Int
doubleArea (p0:ps) = doubleAreaUtility 0 p0 (p0:ps)
doubleArea _ = 0