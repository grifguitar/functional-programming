module Block2Spec
  ( block2TestTree,
  ) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Block2.Task1
import Block2.Task2

block2TestTree :: IO TestTree
block2TestTree = testSpec "block2" block2Spec

block2Spec :: Spec
block2Spec = do
  describe "Block2.Task1" $ do
    it "test1: add" $
      eval (Mul (Add (Const 1) (Const 2)) (Const (-3))) `shouldBe` Right (-9)
    it "test2: sub" $
      eval (Mul (Sub (Const 1) (Const 2)) (Const (-3))) `shouldBe` Right 3
    it "test3: mul" $
      eval (Mul (Div (Const 2) (Const 1)) (Const (10))) `shouldBe` Right 20
    it "test4: div" $
      eval (Div (Add (Const 1) (Const 2)) (Const (3))) `shouldBe` Right 1
    it "test5: pow" $
      eval (Pow (Pow (Const 2) (Const 3)) (Const 2)) `shouldBe` Right 64
    it "test6: division by zero" $
      eval (Div (Sub (Const 1) (Const 2)) (Const 0)) `shouldBe` Left DivisionByZero
    it "test7: division by zero" $
      eval (Div (Mul (Const 2) (Const 0)) (Const 0)) `shouldBe` Left DivisionByZero
    it "test8: NegativeExponentiation" $
      eval (Pow (Pow (Const 2) (Const 3)) (Const (-1))) `shouldBe` Left NegativeExponentiation
    it "test9: NegativeExponentiation" $
      eval (Pow (Mul (Const 2) (Const 3)) (Const (-5))) `shouldBe` Left NegativeExponentiation

  describe "Block2.Task2" $ do
    it "test1: moving 4 [1, 5, 3, 8, 7, 9, 6]" $
      moving 4 [1, 5, 3, 8, 7, 9, 6] `shouldBe` [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]
    it "test2: moving 2 [1, 5, 3, 8, 7, 9, 6]" $
      moving 2 [1, 5, 3, 8, 7, 9, 6] `shouldBe` [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]