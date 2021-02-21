module Main
  ( main
  ) where

import Criterion.Main

import Block1.Task1Crit

main :: IO ()
main = defaultMain [ naiveGeometryBench
                   , effectiveGeometryBench
                   ]