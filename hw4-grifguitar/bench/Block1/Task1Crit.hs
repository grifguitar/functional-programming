module Block1.Task1Crit
  ( naiveGeometryBench
  , effectiveGeometryBench
  ) where

import Criterion.Main

import Control.DeepSeq

import qualified Block1.NaiveGeometry as Naive
import qualified Block1.EffectiveGeometry as Effective

naiveGeometryBench :: Benchmark
naiveGeometryBench =
  bgroup "naive geometry" [ bench "perimeter for 10000 points" $ nf Naive.perimeter points1
                          , bench "doubleArea for 10000 points" $ nf Naive.doubleArea points1
                          , bench "perimeter for 100000 points" $ nf Naive.perimeter points2
                          , bench "doubleArea for 100000 points" $ nf Naive.doubleArea points2
                          ]
    where
      points1 = ($!!) id $ replicate 10000 (Naive.Point 1 3)
      points2 = ($!!) id $ replicate 100000 (Naive.Point 1 2)

effectiveGeometryBench :: Benchmark
effectiveGeometryBench =
  bgroup "effective geometry" [ bench "perimeter for 10000 points" $ nf Effective.perimeter points1
                              , bench "doubleArea for 10000 points" $ nf Effective.doubleArea points1
                              , bench "perimeter for 100000 points" $ nf Effective.perimeter points2
                              , bench "doubleArea for 100000 points" $ nf Effective.doubleArea points2
                              , bench "perimeter for max_test" $ nf Effective.perimeter points3
                              , bench "doubleArea for max_test" $ nf Effective.doubleArea points3
                              ]
    where
      points1 = ($!!) id $ replicate 10000 (Effective.Point 1 3)
      points2 = ($!!) id $ replicate 100000 (Effective.Point 1 2)
      points3 = ($!!) id $ replicate 10000000 (Effective.Point 1 3)