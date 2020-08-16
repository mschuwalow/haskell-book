module Puzzle (
  Puzzle,
  freshPuzzle,
  charInWord,
  alreadyGuessed,
  fillInCharacter,
  incorrectGuesses,
  puzzleDone,
  wordToGuess
) where

import Data.List (intersperse)
import Data.Maybe (isJust)

data Puzzle =
  Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ "\nGuessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle word =
  Puzzle word (fmap (\_ -> Nothing) word) []

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar =
  maybe '_' id

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c =
  elem c word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guesses) c =
  elem c guesses

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word filledInSoFar' (c : s)
  where
    filledInSoFar' =
      zipWith (zipper c) word filledInSoFar
    zipper guessed wordChar guessChar =
      if wordChar == guessed
        then Just wordChar
        else guessChar

incorrectGuesses :: Puzzle -> Int
incorrectGuesses puzzle@(Puzzle _ _ guessed) =
  length . filter (not . charInWord puzzle) $ guessed

puzzleDone :: Puzzle -> Bool
puzzleDone (Puzzle _ filledInSoFar _) =
  all isJust filledInSoFar

wordToGuess :: Puzzle -> String
wordToGuess (Puzzle wordToGuess' _ _) =
  wordToGuess'
