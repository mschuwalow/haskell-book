module Ch17.Combinations where

import Control.Applicative (liftA3)

stops :: [Char]
stops = "pbtdkg"

vowels :: [Char]
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
