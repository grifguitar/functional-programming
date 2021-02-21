module Main
  ( main
  ) where

import Test.Tasty
import Block3Spec (block3TestTree)
import Block2Spec (block2TestTree)
import Block1Spec (block1TestTree)

main :: IO ()
main = do
  test1 <- block1TestTree
  test2 <- block2TestTree
  test3 <- block3TestTree
  defaultMain $ testGroup "Tests" [test1, test2, test3]