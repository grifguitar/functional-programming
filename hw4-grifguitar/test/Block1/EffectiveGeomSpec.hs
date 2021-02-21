module Block1.EffectiveGeomSpec
  ( block1EffectiveGeomTestTree
  ) where

import qualified Block1.EffectiveGeometry as Effective
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

block1EffectiveGeomTestTree :: IO TestTree
block1EffectiveGeomTestTree = testSpec "block1 effective geometry" block1EffectiveGeomSpec

block1EffectiveGeomSpec :: Spec
block1EffectiveGeomSpec = do
  describe "effective plus:" $ do
    it "plus (0,0) (0,1) == (0,1)" $
      Effective.plus ep00 ep01 `shouldBe` ep01
    it "plus (1,0) (0,1) == (1,1)" $
      Effective.plus ep10 ep01 `shouldBe` ep11
  describe "effective minus:" $ do
    it "minus (1,0) (0,0) == (1,0)" $
      Effective.minus ep10 ep00 `shouldBe` ep10
    it "minus (1,1) (0,1) == (1,0)" $
      Effective.minus ep11 ep01 `shouldBe` ep10
  describe "effective scalarProduct:" $ do
    it "scalarProduct (1,0) (0,1) == 0" $
      Effective.scalarProduct ep10 ep01 `shouldBe` 0
    it "scalarProduct (1,0) (1,1) == 1" $
      Effective.scalarProduct ep10 ep11 `shouldBe` 1
  describe "effective crossProduct:" $ do
    it "crossProduct (1,0) (0,1) == 1" $
      Effective.crossProduct ep10 ep01 `shouldBe` 1
    it "crossProduct (1,1) (1,0) == -1" $
      Effective.crossProduct ep11 ep10 `shouldBe` -1
  describe "effective perimeter:" $ do
    it "perimeter [(0,0), (1,0), (1,1)] == 3.414" $
      abs (Effective.perimeter [ep00, ep10, ep11] - 3.414) < 0.001 `shouldBe` True
    it "perimeter [(1,0), (1,1), (0,1)] == 3.414" $
      abs (Effective.perimeter [ep10, ep11, ep01] - 3.414) < 0.001 `shouldBe` True
    it "perimeter [(0,0), (2,1), (1,1)] == 4.65" $
      abs (Effective.perimeter [ep00, ep21, ep11] - 4.65) < 0.01 `shouldBe` True
  describe "effective doubleArea:" $ do
    it "doubleArea [(0,0), (1,0), (1,1)] == 1" $
      Effective.doubleArea [ep00, ep10, ep11] == 1 `shouldBe` True
    it "doubleArea [(1,0), (1,1), (0,1)] == 1" $
      Effective.doubleArea [ep10, ep11, ep01] == 1 `shouldBe` True
    it "doubleArea [(0,0), (2,1), (1,1)] == 1" $
      Effective.doubleArea [ep00, ep21, ep11] == 1 `shouldBe` True
  where
    ep00 = Effective.Point 0 0
    ep01 = Effective.Point 0 1
    ep10 = Effective.Point 1 0
    ep11 = Effective.Point 1 1
    ep21 = Effective.Point 2 1