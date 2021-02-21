module Operations
  ( Operations(..)
  ) where

data Operations = Cd FilePath
                | Dir
                | Ls FilePath
                | CreateFolder FilePath
                | Cat FilePath
                | CreateFile FilePath
                | Remove FilePath
                | WriteFile FilePath String
                | FindFile FilePath
                | Inform FilePath
                | Help
                | Exit
                deriving (Eq, Show)