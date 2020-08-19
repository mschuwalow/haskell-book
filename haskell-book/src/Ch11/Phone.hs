module Ch11.Phone where

import Data.Char
import Data.List
import qualified Data.Map as Map

type Digit = Char

type Presses = Int

data Button
  = Button Digit String
  deriving (Show)

buttonDigit :: Button -> Digit
buttonDigit (Button digit _) =
  digit

data Phone
  = Phone [Button]
  deriving (Show)

phone :: Phone
phone =
  Phone
    [ Button '1' "1",
      Button '2' "abc2",
      Button '3' "def3",
      Button '4' "ghi4",
      Button '5' "jkl5",
      Button '6' "mno6",
      Button '7' "pqrs7",
      Button '8' "tuv8",
      Button '9' "wxyz9",
      Button '0' " 0",
      Button '#' "."
    ]

containsCharacter :: Char -> Button -> Bool
containsCharacter x (Button _ xs) =
  (toLower x) `elem` xs

findButton :: Phone -> Char -> Button
findButton (Phone buttons) x =
  head . (filter (containsCharacter x)) $ buttons

pressesFor :: Char -> Button -> Presses
pressesFor x (Button _ xs) =
  (+ 1) . maybe undefined id . findIndex (== x) $ xs

reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps p@(Phone _) x =
  prefix ++ [(buttonDigit button, presses)]
  where
    x' = toLower x
    button = findButton p x'
    presses = pressesFor x' button
    prefix =
      if isUpper x
        then [('*', 1)]
        else []

fingerTaps :: Phone -> String -> Presses
fingerTaps p =
  foldr f 0
  where
    f :: Char -> Int -> Int
    f x acc =
      (foldr (+) 0 . fmap snd . reverseTaps p $ x) + acc

mostPopularDigit :: Phone -> String -> Char
mostPopularDigit p str =
  fst . maximumBy (ordOf) $ taps'
  where
    ordOf x y = compare (snd x) (snd y)
    taps = str >>= (reverseTaps p)
    taps' = Map.toList . foldr (uncurry (Map.insertWith (+))) Map.empty $ taps

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"
  ]
