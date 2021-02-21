{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module AbstractFileSystem
  ( Tree(..)
  , AbstractFileSystem(..)
  , MyException(..)
  , Command
  , cd
  , ls
  , cat
  , getRealFileSystem
  ) where

import Control.Monad.State
import Control.Monad.Except
import Control.Exception
import System.FilePath
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Data.List (isPrefixOf)
import System.Directory
import Data.ByteString.Internal (unpackChars)

data MyException = FileSystemException String | ParserException String deriving (Eq, Show)

instance Exception MyException

data Tree = Directory FilePath (Map.Map FilePath Tree) | File FilePath BS.ByteString deriving (Eq)

instance Show Tree where
  show :: Tree -> String
  show (File x y) = "(file: " ++ show x ++ " " ++ show (take 20 (unpackChars y) ++ "..") ++ ")"
  show (Directory x y) = "(dir: " ++ show x ++ " " ++ showMap y ++ ")"

showMap :: Map.Map FilePath Tree -> String
showMap mp = show $ map snd (Map.toList mp)

getTreePath :: Tree -> FilePath
getTreePath (Directory path _) = path
getTreePath (File path _) = path

data AbstractFileSystem = AbstractFileSystem { getRootTree :: Tree , getCurrTree :: Tree } deriving (Eq)

instance Show AbstractFileSystem where
  show :: AbstractFileSystem -> String
  show fs = "{ " ++ getTreePath (getRootTree fs) ++ " , " ++ getTreePath (getCurrTree fs) ++ " }"

type Command a = ExceptT MyException (StateT AbstractFileSystem IO) a

cd :: FilePath -> Command ()
cd notNormPath = do
  st <- get
  let path = normalise notNormPath
  let rootTree = getRootTree st
  let currTree = getCurrTree st
  let rootPath = getTreePath rootTree
  let currPath = getTreePath currTree
  let pp = splitDirectories (makeRelative rootPath currPath)
  --liftIO (print path)
  --liftIO (print pp)
  let maybeP
        | path == ".." = if pp /= []
                         then Just $ Left (init pp)
                         else Nothing
        | isRelative path = Just $ Right (splitDirectories path)
        | currPath `isPrefixOf` path = Just $ Right(splitDirectories (makeRelative currPath path))
        | rootPath `isPrefixOf` path = Just $ Left (splitDirectories (makeRelative rootPath path))
        | otherwise = Nothing
  --liftIO (print maybeP)
  let maybeT = case maybeP of
                  Just (Right p) -> getMaybeTree p currTree
                  Just (Left p) -> getMaybeTree p rootTree
                  Nothing -> Nothing
  --liftIO (print maybeT)
  let fs = case maybeT of
              Just t -> AbstractFileSystem rootTree t
              Nothing -> st
  case maybeP of
    Nothing -> throwError (FileSystemException "incorrect path")
    Just _ -> case maybeT of
              Nothing -> throwError (FileSystemException "not found")
              Just _ -> do
                put fs

ls :: FilePath -> Command ()
ls notNormPath = do
  st1 <- get
  let currTree1 = getCurrTree st1
  let currPath1 = getTreePath currTree1
  cd notNormPath
  st2 <- get
  let currTree2 = getCurrTree st2
  case currTree2 of
    (Directory _ mp) -> liftIO (print mp)
    (File _ _) -> throwError (FileSystemException "not file")
  cd currPath1

cat :: FilePath -> Command ()
cat notNormPath = do
  st1 <- get
  let currTree1 = getCurrTree st1
  let currPath1 = getTreePath currTree1
  cd notNormPath
  st2 <- get
  let currTree2 = getCurrTree st2
  case currTree2 of
    (Directory _ _) -> throwError (FileSystemException "not directory")
    (File _ fileData) -> liftIO (print fileData)
  cd currPath1


getMaybeTree :: [FilePath] -> Tree -> Maybe Tree
getMaybeTree (x:xs) t@(Directory currPath mp) =
  if x == "."
  then Just t
  else case Map.lookup (currPath </> x) mp of
          Nothing -> Nothing
          Just tree -> getMaybeTree xs tree
getMaybeTree [] tree = Just tree
getMaybeTree _ _ = Nothing

getRealTree :: FilePath -> IO Tree
getRealTree path = do
  isFileExist <- doesFileExist path
  case isFileExist of
    False -> do
        neighbors <- listDirectory path
        fmap (Directory path) (foldr f mempty neighbors)
        where
          f = liftM2 (\x y -> Map.insert (getTreePath x) x y) . getRealTree . (</>) path
    True -> do
        fileData <- BS.readFile path
        return (File path fileData)

getRealFileSystem :: IO AbstractFileSystem
getRealFileSystem = do
  rootPath <- getCurrentDirectory
  tree <- getRealTree rootPath
  return (AbstractFileSystem tree tree)