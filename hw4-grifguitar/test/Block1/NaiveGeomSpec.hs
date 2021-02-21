module Block1.NaiveGeomSpec
  ( block1NaiveGeomTestTree
  ) where

import qualified Block1.NaiveGeometry as Naive
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

block1NaiveGeomTestTree :: IO TestTree
block1NaiveGeomTestTree = testSpec "block1 naive geometry" block1NaiveGeomSpec

block1NaiveGeomSpec :: Spec
block1NaiveGeomSpec = do
  describe "naive plus:" $ do
    it "plus (0,0) (0,1) == (0,1)" $
      Naive.plus np00 np01 `shouldBe` np01
    it "plus (1,0) (0,1) == (1,1)" $
      Naive.plus np10 np01 `shouldBe` np11
  describe "naive minus:" $ do
    it "minus (1,0) (0,0) == (1,0)" $
      Naive.minus np10 np00 `shouldBe` np10
    it "minus (1,1) (0,1) == (1,0)" $
      Naive.minus np11 np01 `shouldBe` np10
  describe "naive scalarProduct:" $ do
    it "scalarProduct (1,0) (0,1) == 0" $
      Naive.scalarProduct np10 np01 `shouldBe` 0
    it "scalarProduct (1,0) (1,1) == 1" $
      Naive.scalarProduct np10 np11 `shouldBe` 1
  describe "naive crossProduct:" $ do
    it "crossProduct (1,0) (0,1) == 1" $
      Naive.crossProduct np10 np01 `shouldBe` 1
    it "crossProduct (1,1) (1,0) == -1" $
      Naive.crossProduct np11 np10 `shouldBe` -1
  describe "naive perimeter:" $ do
    it "perimeter [(0,0), (1,0), (1,1)] == 3.414" $
      abs (Naive.perimeter [np00, np10, np11] - 3.414) < 0.001 `shouldBe` True
    it "perimeter [(1,0), (1,1), (0,1)] == 3.414" $
      abs (Naive.perimeter [np10, np11, np01] - 3.414) < 0.001 `shouldBe` True
    it "perimeter [(0,0), (2,1), (1,1)] == 4.65" $
      abs (Naive.perimeter [np00, np21, np11] - 4.65) < 0.01 `shouldBe` True
  describe "naive doubleArea:" $ do
    it "doubleArea [(0,0), (1,0), (1,1)] == 1" $
      Naive.doubleArea [np00, np10, np11] == 1 `shouldBe` True
    it "doubleArea [(1,0), (1,1), (0,1)] == 1" $
      Naive.doubleArea [np10, np11, np01] == 1 `shouldBe` True
    it "doubleArea [(0,0), (2,1), (1,1)] == 1" $
      Naive.doubleArea [np00, np21, np11] == 1 `shouldBe` True
  where
    np00 = Naive.Point 0 0
    np01 = Naive.Point 0 1
    np10 = Naive.Point 1 0
    np11 = Naive.Point 1 1
    np21 = Naive.Point 2 1