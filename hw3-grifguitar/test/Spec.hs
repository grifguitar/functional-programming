module Main
  ( main
  ) where

import Test.Tasty
import Block1Spec (block1TestTree)

main :: IO ()
main = do
  test1 <- block1TestTree
  defaultMain $ testGroup "Tests" [test1]