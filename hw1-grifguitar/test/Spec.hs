module Main
  ( main
  ) where

import Test.Tasty
import Block1Task1Spec (block1Task1TestTree)
import Block1Task2Spec (block1Task2TestTree)
import Block1Task3Spec (block1Task3TestTree)
import Block2Task1Spec (block2Task1TestTree)

main :: IO ()
main = do
  test1 <- block1Task1TestTree
  test2 <- block1Task2TestTree
  test3 <- block1Task3TestTree
  test4 <- block2Task1TestTree
  defaultMain $ testGroup "Tests" [test1, test2, test3, test4]