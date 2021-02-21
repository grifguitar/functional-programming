module Block3Spec
  ( block3TestTree,
  ) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

block3TestTree :: IO TestTree
block3TestTree = testSpec "block3" block3Spec

block3Spec :: Spec
block3Spec = do
  describe "Block3.Task1" $ do
    it "test1" $
      True `shouldBe` True

  describe "Block3.Task2" $ do
    it "test1" $
      True `shouldBe` True

  describe "Block3.Task3" $ do
    it "test1" $
      True `shouldBe` True

  describe "Block3.Task4" $ do
    it "test1" $
      True `shouldBe` True