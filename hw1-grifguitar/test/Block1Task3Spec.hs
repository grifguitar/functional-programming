module Block1Task3Spec
  ( block1Task3TestTree,
  ) where

import Block1.Task3
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import qualified Data.List.NonEmpty as List

block1Task3TestTree :: IO TestTree
block1Task3TestTree = testSpec "block1Task3" block1Task3Spec

block1Task3Spec :: Spec
block1Task3Spec = do
  describe "Block1.Task3.isEmpty" $ do
    it "isEmpty (fromList [] :: Node Int) == True" $
      isEmpty (fromList [] :: Node Int) `shouldBe` True
    it "isEmpty (fromList [1, 2] :: Node Int) == False" $
      isEmpty (fromList [1, 2] :: Node Int) `shouldBe` False
    it "isEmpty (fromList [] :: Node Bool) `shouldBe` True" $
      isEmpty (fromList [] :: Node Bool) `shouldBe` True
    it "isEmpty (fromList [True, False] :: Node Bool) `shouldBe` False" $
      isEmpty (fromList [True, False] :: Node Bool) `shouldBe` False

  describe "Block1.Task3.getElementsAmount" $ do
    it "getElementsAmount (fromList [1, 5, 2] :: Node Int) `shouldBe` 3" $
      getElementsAmount (fromList [1, 5, 2] :: Node Int) `shouldBe` 3
    it "getElementsAmount (fromList [] :: Node Int) `shouldBe` 0" $
      getElementsAmount (fromList [] :: Node Int) `shouldBe` 0
    it "getElementsAmount (fromList [one, five] :: Node String) `shouldBe` 2" $
      getElementsAmount (fromList ["one", "five"] :: Node String) `shouldBe` 2

  describe "Block1.Task3.Insert" $ do
    it "insert 1 to [] equals [1]" $
      insertElement 1 (fromList [] :: Node Int) `shouldBe` MT ((1 :: Int) List.:| []) ZZ ZZ

  describe "Block1.Task3.Remove" $ do
    it "remove 5 to [4, 5, 100] equals [4, 100]" $
      removeElement 5 (fromList [4, 5, 100] :: Node Int) `shouldBe` (fromList [4, 100] :: Node Int)
    it "remove 0 to [0] equals []" $
      removeElement 0 (MT ((0 :: Int) List.:| []) ZZ ZZ) `shouldBe` ZZ

  describe "Block1.Task3.Find" $ do
    it "findElement 10 (fromList [1, 0, 100, 1000] :: Node Int) `shouldBe` False" $
      findElement 10 (fromList [1, 0, 100, 1000] :: Node Int) `shouldBe` False
    it "findElement 11 (fromList [1, 0, 11, 15, 21] :: Node Int) `shouldBe` True" $
      findElement 11 (fromList [1, 0, 11, 15, 21] :: Node Int) `shouldBe` True
    it "findElement 0 (fromList [] :: Node Int) `shouldBe` False" $
      findElement 0 (fromList [] :: Node Int) `shouldBe` False

  describe "Block1.Task3.FromList" $ do
    it "fromList [1001] equals Tree: [left: ] -- [1001] -- [right: ]" $
      (fromList [1001] :: Node Int) `shouldBe` MT ((1001 :: Int) List.:| []) ZZ ZZ
    it "fromList [] equals Tree: []" $
      (fromList [] :: Node Int) `shouldBe` ZZ