module Main where

import Control.Monad.State
import Control.Monad.Except

import Parser
import Operations
import AbstractFileSystem

-- Imports for test file system:

--import qualified Data.Map as Map
--import Data.ByteString.Internal (packChars)

whileNotExit :: Command ()
whileNotExit = do
    line <- liftIO getLine

    case runParser operationsParser line of
      Just (Exit, _) -> return ()
      Just (Cd path, _) -> do
        fs1 <- get
        liftIO (print fs1)
        catchError (cd path) (liftIO . print)
        fs2 <- get
        liftIO (print fs2)
        whileNotExit
      Just (Ls path, _) -> do
        fs1 <- get
        liftIO (print fs1)
        catchError (ls path) (liftIO . print)
        fs2 <- get
        liftIO (print fs2)
        whileNotExit
      Just (Cat path, _) -> do
        fs1 <- get
        liftIO (print fs1)
        catchError (cat path) (liftIO . print)
        fs2 <- get
        liftIO (print fs2)
        whileNotExit
      Just (x, _) -> do
        liftIO (print x)
        whileNotExit
      Nothing -> do
        liftIO (print "there is no such command")
        whileNotExit

main :: IO ()
main = do
--  Test file system example:

--  let mp1 = Map.fromList [("C:\\1\\2", File "C:\\1\\2" (packChars "LEAF2")),("C:\\1\\4", File "C:\\1\\4" (packChars "LEAF4"))]
--  let tree = Directory "C:\\" $ Map.fromList [("C:\\1", Directory "C:\\1" mp1),("C:\\10", File "C:\\10" (packChars "LEAF10"))]
--  _ <- execStateT (runExceptT whileNotExit) (AbstractFileSystem tree tree)
--  return ()

--  Real file system example:

  fs <- getRealFileSystem
  _ <- execStateT (runExceptT whileNotExit) fs
  return ()