module Block1Task2Spec
  ( block1Task2TestTree
  ) where

import Block1.Task2
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

block1Task2TestTree :: IO TestTree
block1Task2TestTree = testSpec "block1Task2" block1Task2Spec

block1Task2Spec :: Spec
block1Task2Spec = do
  describe "Block1.Task2.equality" $ do
    it "check: one is equal to one?" $
      natOne == natOne `shouldBe` True
    it "check: two is equal to two?" $
      natTwo == natTwo `shouldBe` True
    it "check: zero is equal to zero?" $
      natZero == natZero `shouldBe` True
    it "check: one is not equal to two?" $
      natOne == natTwo `shouldBe` False
    it "check: one is not equal to zero?" $
      natOne /= natZero `shouldBe` True

  describe "Block1.Task2.comparation" $ do
    it "check: zero less than one?" $
      natZero < natOne `shouldBe` True
    it "check: one is not less than zero?" $
      natZero > natOne `shouldBe` False

  describe "Block1.Task2.summation" $ do
    it "check: 2 + 2 == 4?" $
      natTwo + natTwo == natFour `shouldBe` True
    it "check: 2 + 0 == 2?" $
      natTwo + natZero == natTwo `shouldBe` True
    it "check: 0 + 0 /= 1?" $
      natZero + natZero == natOne `shouldBe` False
    it "check: 1 + 2 == 3?" $
      natOne + natTwo == natThree `shouldBe` True

  describe "Block1.Task2.multiplication" $ do
    it "check: 1 * 2 == 2?" $
      natOne * natTwo == natTwo `shouldBe` True
    it "check: 0 * 10 == 0?" $
      natZero * natTen == natZero `shouldBe` True
    it "check: 2 * 2 == 4?" $
      natTwo * natTwo == natFour `shouldBe` True

  describe "Block1.Task2.fromIntegerAndToInteger" $ do
    it "check: fromInteger(1) == one" $
      natOne == (fromInteger 1) `shouldBe` True
    it "check: toInteger(one) == 1" $
      (toInteger natOne) == 1 `shouldBe` True
    it "check: fromInteger(2) /= one" $
      natOne /= (fromInteger 2) `shouldBe` True
    it "check: toInteger(one) /= 0" $
      (toInteger natOne) == 0 `shouldBe` False

  describe "Block1.Task2.subtraction" $ do
    it "check: 2 - 1 == 1?" $
      natTwo - natOne == natOne `shouldBe` True
    it "check: 1 - 1 == 0?" $
      natOne - natOne == natZero `shouldBe` True
    it "check: 1 - 3 /= 4?" $
      natOne - natThree == natFour `shouldBe` False
    it "check: 0 - 0 == 0?" $
      natZero - natZero == natZero `shouldBe` True

  describe "Block1.Task2.advanced" $ do
    it "check: 4 div 2 == 2?" $
      div natFour natTwo == natTwo `shouldBe` True
    it "check: 4 div 3 /= 2?" $
      div natFour natThree == natTwo `shouldBe` False
    it "check: 1 div 3 == 0?" $
      div natOne natThree == natZero `shouldBe` True
    it "check: two is even?" $
      even natTwo `shouldBe` True
    it "check: one is not even?" $
      even natOne `shouldBe` False
    it "check: zero is even?" $
      even natZero `shouldBe` True

  where
    natZero = Z
    natOne = S Z
    natTwo = S (S Z)
    natThree = S (S (S Z))
    natFour = S (S (S (S Z)))
    natTen = S (S (S (S (S (S (S (S (S (S Z)))))))))