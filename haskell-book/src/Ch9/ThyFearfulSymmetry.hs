module Ch9.ThyFearfulSymmetry where

myWords :: String -> [String]
myWords [] = []
myWords xs =
  word : (myWords rest)
  where
    withoutWhiteSpace = dropWhile (== ' ') xs
    word = takeWhile (/= ' ') $ withoutWhiteSpace
    rest = dropWhile (/= ' ') $ withoutWhiteSpace
