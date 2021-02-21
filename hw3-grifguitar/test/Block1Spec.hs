module Block1Spec
  ( block1TestTree,
  ) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Control.Monad.State
import Control.Monad.Except
import Parser
import Operations
import AbstractFileSystem
import Data.ByteString.Internal (packChars)
import qualified Data.Map as Map

block1TestTree :: IO TestTree
block1TestTree = testSpec "block1" block1Spec

block1Spec :: Spec
block1Spec = do
  describe "cd tests:" $ do
    it "test1: cd 1" $ do
      res <- runStateT (runExceptT (action "cd 1")) (AbstractFileSystem initTree initTree)
      res `shouldBe` (Right (Just (AbstractFileSystem initTree treeCd1)), AbstractFileSystem initTree treeCd1)
    it "test2: cd 2" $ do
      res <- runStateT (runExceptT (action "cd 2")) (AbstractFileSystem initTree treeCd1)
      res `shouldBe` (Right (Just (AbstractFileSystem initTree treeCd1Cd2)), AbstractFileSystem initTree treeCd1Cd2)
    it "test3: cd .." $ do
      res <- runStateT (runExceptT (action "cd ..")) (AbstractFileSystem initTree treeCd1Cd2)
      res `shouldBe` (Right (Just (AbstractFileSystem initTree treeCd1)), AbstractFileSystem initTree treeCd1)
    it "test4: cd 4" $ do
      res <- runStateT (runExceptT (action "cd 4")) (AbstractFileSystem initTree treeCd1)
      res `shouldBe` (Right (Just (AbstractFileSystem initTree treeCd1Cd4)), AbstractFileSystem initTree treeCd1Cd4)
    it "test5: cd .." $ do
      res <- runStateT (runExceptT (action "cd ..")) (AbstractFileSystem initTree treeCd1Cd4)
      res `shouldBe` (Right (Just (AbstractFileSystem initTree treeCd1)), AbstractFileSystem initTree treeCd1)
    it "test6: cd .." $ do
      res <- runStateT (runExceptT (action "cd ..")) (AbstractFileSystem initTree treeCd1)
      res `shouldBe` (Right (Just (AbstractFileSystem initTree initTree)), AbstractFileSystem initTree initTree)
    it "test7: cd .." $ do
      res <- runStateT (runExceptT (action "cd ..")) (AbstractFileSystem initTree initTree)
      res `shouldBe` (Right (Just (AbstractFileSystem initTree initTree)), AbstractFileSystem initTree initTree)
    it "test8: cd C:\\1\\2" $ do
      res <- runStateT (runExceptT (action "cd C:\\1\\2")) (AbstractFileSystem initTree initTree)
      res `shouldBe` (Right (Just (AbstractFileSystem initTree treeCd1Cd2)), AbstractFileSystem initTree treeCd1Cd2)
    it "test9: cd C:\\" $ do
      res <- runStateT (runExceptT (action "cd C:\\")) (AbstractFileSystem initTree treeCd1Cd2)
      res `shouldBe` (Right (Just (AbstractFileSystem initTree initTree)), AbstractFileSystem initTree initTree)
    it "test10: cd E:\\" $ do
      res <- runStateT (runExceptT (action "cd E:\\")) (AbstractFileSystem initTree initTree)
      res `shouldBe` (Left (FileSystemException "incorrect path"), AbstractFileSystem initTree initTree)
      liftIO (print ("#INFORMATION# command: " ++ "cd E:\\" ++
                     " # before: " ++ show (AbstractFileSystem initTree initTree) ++
                     " # after: " ++ show (AbstractFileSystem initTree initTree) ++
                     " # exception: " ++ show (FileSystemException "incorrect path")))
    it "test11: cd x" $ do
      res <- runStateT (runExceptT (action "cd x")) (AbstractFileSystem initTree initTree)
      res `shouldBe` (Left (FileSystemException "not found"), AbstractFileSystem initTree initTree)
      liftIO (print ("#INFORMATION# command: " ++ "cd x" ++
                     " # before: " ++ show (AbstractFileSystem initTree initTree) ++
                     " # after: " ++ show (AbstractFileSystem initTree initTree) ++
                     " # exception: " ++ show (FileSystemException "not found")))

  where
    file2 :: Tree
    file2 = File "C:\\1\\2" (packChars "LEAF2")
    file4 :: Tree
    file4 = File "C:\\1\\4" (packChars "LEAF4")
    file10 :: Tree
    file10 = File "C:\\10" (packChars "LEAF10")
    mp1 :: Map.Map FilePath Tree
    mp1 = Map.fromList [("C:\\1\\2", file2),("C:\\1\\4", file4)]
    rootMap :: Map.Map FilePath Tree
    rootMap = Map.fromList [("C:\\1", Directory "C:\\1" mp1),("C:\\10", file10)]
    initTree :: Tree
    initTree = Directory "C:\\" rootMap
    treeCd1 :: Tree
    treeCd1 = Directory "C:\\1" mp1
    treeCd1Cd2 :: Tree
    treeCd1Cd2 = file2
    treeCd1Cd4 :: Tree
    treeCd1Cd4 = file4

    action :: String -> Command (Maybe AbstractFileSystem)
    action line = do
        case runParser operationsParser line of
          Just (Exit, _) -> return Nothing
          Just (Cd path, _) -> do
            fs1 <- get
            cd path
            fs2 <- get
            liftIO (print ("#INFORMATION# command: " ++ line ++ " # before: " ++ show fs1 ++ " # after: " ++ show fs2))
            return (Just fs2)
          Just (_, _) -> do
            return Nothing
          Nothing -> do
            return Nothing