module Block3.Task4
  ( listListParser
  ) where

import Block3.Task3
import Block3.Task2
import Control.Applicative

spacesParser :: Parser Char String
spacesParser = many (element ' ')

ignoreCommaAndSpaces :: Parser Char a -> Parser Char a
ignoreCommaAndSpaces parser = spacesParser *> (element ',') *> spacesParser *> parser

countIntParse :: Int -> Parser Char [Int]
countIntParse count
  | count > 0  = (<*>) (fmap (:) parser) (countIntParse (count - 1))
  | count == 0 = pure []
  | otherwise  = empty
    where
      parser = (ignoreCommaAndSpaces intParser)

listParser :: Parser Char [Int]
listParser = intParser >>= countIntParse

manyListParse :: Parser Char [[Int]]
manyListParse = (<|>) ((<*>) (fmap (:) listParser) (many (ignoreCommaAndSpaces listParser)))
                      (fmap (const []) ok)

emptyListParse :: Parser Char [a]
emptyListParse = (<*) (fmap (const []) spacesParser) eof

listListParser :: Parser Char [[Int]]
listListParser = (<|>) emptyListParse
                       (spacesParser *> manyListParse <* spacesParser <* eof)