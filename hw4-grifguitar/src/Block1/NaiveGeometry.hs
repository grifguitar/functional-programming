{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Block1.NaiveGeometry
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
abstractPointsOperation op (Point x1 y1) (Point x2 y2) = Point (op x1 x2) (op y1 y2)

pointToInt :: Op -> Point -> Int
pointToInt op (Point x y) = op x y

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
vectorLength p1 p2 = sqrt $ fromIntegral $ scalarProduct p p
  where
    p :: Point
    p = minus p1 p2

perimeterUtility :: Point -> [Point] -> Double
perimeterUtility p0 (p1:p2:ps) = vectorLength p1 p2 + perimeterUtility p0 (p2:ps)
perimeterUtility p0 (p1:_) = vectorLength p1 p0
perimeterUtility _ _ = 0

doubleAreaUtility :: Point -> [Point] -> Int
doubleAreaUtility p0 (p1:p2:ps) = crossProduct p1 p2 + doubleAreaUtility p0 (p2:ps)
doubleAreaUtility p0 (p1:_) = crossProduct p1 p0
doubleAreaUtility _ _ = 0

perimeter :: [Point] -> Double
perimeter (p0:ps) = perimeterUtility p0 (p0:ps)
perimeter _ = 0

doubleArea :: [Point] -> Int
doubleArea (p0:ps) = doubleAreaUtility p0 (p0:ps)
doubleArea _ = 0