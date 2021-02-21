module Block1Spec
  ( block1TestTree,
  ) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Block1.Task1
import Block1.Task2

block1TestTree :: IO TestTree
block1TestTree = testSpec "block1" block1Spec

block1Spec :: Spec
block1Spec = do
  describe "Block1.Task1" $ do
    it "test1: empty string test" $
      stringSum "" == (Just 0) `shouldBe` True
    it "test2: one positive integer test" $
      stringSum "1" == (Just 1) `shouldBe` True
    it "test3: one negative integer test" $
      stringSum "-1" == (Just (-1)) `shouldBe` True
    it "test4: integers with many spaces test" $
      stringSum "  0     1 2   -1   3   -2 -3   " == (Just 0) `shouldBe` True
    it "test5: random test" $
      stringSum "4  29  -61 1   39 87 65   31 10 18" == (Just 223) `shouldBe` True
    it "test6: integers and letter test" $
      stringSum "  1   2   3  b  " == (Nothing) `shouldBe` True
    it "test7: letters test" $
      stringSum " a  b   c  d  " == (Nothing) `shouldBe` True

  describe "Block1.Task2" $ do
    it "test1: functor laws: fmap id = id for Tree Int" $
      fmap id intTree `shouldBe` id intTree
    it "test2: functor laws: fmap id = id for Tree Double" $
      fmap id doubleTree `shouldBe` id doubleTree
    it "test3: functor laws: fmap (f . g) = fmap f . fmap g for Tree Int" $
      fmap (fIncInt . fDecInt) intTree `shouldBe`
        (fmap fIncInt . fmap fDecInt) intTree
    it "test4: functor laws: fmap (f . g) = fmap f . fmap g for Tree Double" $
      fmap (fIncDouble . fDecDouble) doubleTree `shouldBe`
        (fmap fIncDouble . fmap fDecDouble) doubleTree
    it "test5: applicative laws: identity for Tree Int" $
      pure id <*> intTree `shouldBe` intTree
    it "test6: applicative laws: composition for Tree Int" $
      pure (.) <*> (Leaf fIncInt) <*> (Leaf fDecInt) <*> (intTree) `shouldBe`
        (Leaf fIncInt) <*> ((Leaf fDecInt) <*> (intTree))
    it "test7: applicative laws: homomorphism for Tree Int" $
     (pure fIncInt) <*> (Leaf 1) `shouldBe` (Leaf 2)
    it "test8: applicative laws: interchange for Tree Int" $
      (Leaf fIncInt) <*> (pure 1) `shouldBe`
        pure ($ 1) <*> (Leaf fIncInt)
    it "test9: fmap Tree test" $
      fmap fIncInt (Leaf 1) `shouldBe` (Leaf 2)
    it "test10: <*> Tree test" $
      (Leaf fIncInt) <*> (Leaf 1) `shouldBe` (Leaf 2)
    it "test11: foldl Tree test" $
      foldl (+) 0 intTree `shouldBe` 4
    it "test12: foldr Tree test" $
      foldr (+) 0 intTree `shouldBe` 4

  where
    intTree :: Tree Int
    intTree = (Branch (Leaf (-1)) (Branch (Leaf 2) (Leaf 3)))
    doubleTree :: Tree Double
    doubleTree = (Branch (Leaf 1.1) (Branch (Leaf (-2.2)) (Leaf 3.3)))
    fIncInt :: Int -> Int
    fIncInt x = x + 1
    fDecInt :: Int -> Int
    fDecInt x = x - 1
    fIncDouble :: Double -> Double
    fIncDouble x = x + 1
    fDecDouble :: Double -> Double
    fDecDouble x = x - 1