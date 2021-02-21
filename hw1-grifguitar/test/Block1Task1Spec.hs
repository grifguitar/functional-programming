module Block1Task1Spec
  ( block1Task1TestTree
  ) where

import Block1.Task1
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

block1Task1TestTree :: IO TestTree
block1Task1TestTree = testSpec "block1Task1" block1Task1Spec

block1Task1Spec :: Spec
block1Task1Spec = do
  describe "Block1.Task1.nextDay" $ do
    it "returns the next day after Monday" $
      nextDay Sunday `shouldBe` Monday
    it "returns the next day after Monday" $
      nextDay Saturday `shouldBe` Sunday
    it "returns the next day after Monday" $
      nextDay Friday `shouldBe` Saturday
    it "returns the next day after Monday" $
      nextDay Thursday `shouldBe` Friday
    it "returns the next day after Monday" $
      nextDay Wednesday `shouldBe` Thursday
    it "returns the next day after Monday" $
      nextDay Tuesday `shouldBe` Wednesday
    it "returns the next day after Monday" $
      nextDay Monday `shouldBe` Tuesday

  describe "Block1.Task1.afterDays" $ do
    it "returns 1 day after Monday" $
      afterDays Monday 1 `shouldBe` Tuesday
    it "returns 2 day after Monday" $
      afterDays Monday 2 `shouldBe` Wednesday
    it "returns 3 day after Wednesday" $
      afterDays Wednesday 3 `shouldBe` Saturday
    it "returns 6 day after Tuesday" $
      afterDays Tuesday 6 `shouldBe` Monday
    it "returns 7 day after Tuesday" $
      afterDays Tuesday 7 `shouldBe` Tuesday
    it "returns 8 day after Tuesday" $
      afterDays Tuesday 8 `shouldBe` Wednesday
    it "returns 100 day after Monday" $
      afterDays Monday 100 `shouldBe` Wednesday
    it "returns 0 day after Friday" $
      afterDays Friday 0 `shouldBe` Friday

  describe "Block1.Task1.isWeekend" $ do
    it "returns true if Sunday is weekend" $
      isWeekend Sunday `shouldBe` True
    it "returns true if Saturday is weekend" $
      isWeekend Saturday `shouldBe` True
    it "returns true if Friday is weekend" $
      isWeekend Friday `shouldBe` False
    it "returns true if Thursday is weekend" $
      isWeekend Thursday `shouldBe` False
    it "returns true if Wednesday is weekend" $
      isWeekend Wednesday `shouldBe` False
    it "returns true if Tuesday is weekend" $
      isWeekend Tuesday `shouldBe` False
    it "returns true if Monday is weekend" $
      isWeekend Monday `shouldBe` False
  
  describe "Block1.Task1.daysToParty" $ do
    it "returns how many days from Monday to Friday" $
      daysToParty Sunday `shouldBe` 5
    it "returns how many days from Monday to Friday" $
      daysToParty Saturday `shouldBe` 6
    it "returns how many days from Monday to Friday" $
      daysToParty Friday `shouldBe` 0
    it "returns how many days from Monday to Friday" $
      daysToParty Thursday `shouldBe` 1
    it "returns how many days from Monday to Friday" $
      daysToParty Wednesday `shouldBe` 2
    it "returns how many days from Monday to Friday" $
      daysToParty Tuesday `shouldBe` 3
    it "returns how many days from Monday to Friday" $
      daysToParty Monday `shouldBe` 4