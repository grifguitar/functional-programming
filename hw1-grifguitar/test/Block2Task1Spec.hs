module Block2Task1Spec
  ( block2Task1TestTree,
  ) where

import Block2.Task1
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Data.Foldable(toList);

block2Task1TestTree :: IO TestTree
block2Task1TestTree = testSpec "block2Task1" block2Task1Spec

block2Task1Spec :: Spec
block2Task1Spec = do
  describe "Block2.Task1.FoldableTests" $ do
    it "toList . fromList == sort for [1, 0, 15, 11, 21]" $
      toList (fromList [1, 0, 15, 11, 21] :: Node Int) `shouldBe` ([0, 1, 11, 15, 21] :: [Int])
    it "toList . fromList == sort for [90, 17, 72, 0, 75]" $
      toList (fromList [90, 17, 72, 0, 75] :: Node Int) `shouldBe` ([0, 17, 72, 75, 90] :: [Int])
    it "toList . fromList == sort for [32, 25, 79, 61, 54]" $
      toList (fromList [32, 25, 79, 61, 54] :: Node Int) `shouldBe` ([25, 32, 54, 61, 79] :: [Int])
    it "toList . fromList == sort for [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0]" $
      toList (fromList [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0] :: Node Int) `shouldBe`
      ([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10] :: [Int])