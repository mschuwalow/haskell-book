module Words (
  randomWord
) where

import Paths_hangman (getDataFileName)
import System.Random (randomRIO)

newtype WordList = WordList {
  unWordList :: [String]
} deriving (Eq, Show)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
  path <- getDataFileName "data/dict.txt"
  dict <- readFile path
  return $ WordList (lines dict)

gameWords :: WordList -> WordList
gameWords =
  WordList . filter gameLength . unWordList
  where
    gameLength w =
      let l = length w
      in     l >= minWordLength
          && l <  maxWordLength

selectRandomWord :: WordList -> IO String
selectRandomWord wl = do
  let wl' = unWordList wl
  randomIndex <- randomRIO (0, (subtract 1) . length $ wl')
  return $ wl' !! randomIndex

randomWord :: IO String
randomWord = fmap gameWords allWords >>= selectRandomWord
