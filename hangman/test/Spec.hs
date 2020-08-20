{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Game
import Puzzle
import Test.Hspec
import Test.QuickCheck

instance (Arbitrary Puzzle) where
  arbitrary = do
    word <- arbitrary @String
    return $ freshPuzzle word

prop_filledInSoFar :: Puzzle -> Char -> Bool
prop_filledInSoFar puzzle c
  | charInWord puzzle c = (incorrectGuesses puzzle' == 0) && (alreadyGuessed puzzle' c)
  | otherwise = (incorrectGuesses puzzle' == 1) && (alreadyGuessed puzzle' c)
  where
    puzzle' = fillInCharacter puzzle c

main :: IO ()
main = hspec $ do
  describe "Puzzle" $ do
    it "should update when guessing" $ do
      property prop_filledInSoFar
    it "should win game when filling in all words"
      $ do
        (flip shouldBe) True
        . puzzleDone
        . (flip fillInCharacter) 't'
        . (flip fillInCharacter) 's'
        . (flip fillInCharacter) 'e'
        . (flip fillInCharacter) 't'
      $ freshPuzzle "test"
  describe "handleGuess" $ do
    it "should not change puzzle on identical guess" $
      let test = do
            let puzzle = freshPuzzle "test"
            puzzle' <- handleGuess puzzle 't'
            puzzle'' <- handleGuess puzzle' 't'
            return $ puzzle' == puzzle''
       in test `shouldReturn` True
