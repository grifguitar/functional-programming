module Main
  ( main
  ) where

import Test.Tasty
import Block1.NaiveGeomSpec (block1NaiveGeomTestTree)
import Block1.EffectiveGeomSpec (block1EffectiveGeomTestTree)

main :: IO ()
main = do
  block1NaiveGeomTest <- block1NaiveGeomTestTree
  block1EffectiveGeomTest <- block1EffectiveGeomTestTree
  defaultMain $ testGroup "Tests" [ block1NaiveGeomTest
                                  , block1EffectiveGeomTest
                                  ]