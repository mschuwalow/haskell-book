module Main where

import Data.Char (toLower)
import Puzzle
import System.Exit (exitSuccess)
import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout)
import Words

maxIncorrectGuesses :: Int
maxIncorrectGuesses = 7

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle

runGame :: Puzzle -> IO ()
runGame puzzle = do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStrLn $ "Incorrect guesses: " ++ (show . incorrectGuesses $ puzzle) ++ "/" ++ (show maxIncorrectGuesses)
  putStr "\nGuess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character" >> runGame puzzle

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess =
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
              \ character, pick\
              \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the\
              \ word, filling in the word\
              \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
              \ the word, try again."
      return (fillInCharacter puzzle guess)

gameWin :: Puzzle -> IO ()
gameWin puzzle =
  if puzzleDone puzzle
  then do
    putStrLn "You win!"
    putStrLn $ "The word is: " ++ wordToGuess puzzle
    exitSuccess
  else return ()

gameOver :: Puzzle -> IO ()
gameOver puzzle =
  if (incorrectGuesses puzzle) > maxIncorrectGuesses
  then do
    putStrLn "You lose!"
    putStrLn $ "The word was: " ++ wordToGuess puzzle
    exitSuccess
  else return ()
