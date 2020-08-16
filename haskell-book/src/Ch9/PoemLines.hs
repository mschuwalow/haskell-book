module Ch9.PoemLines where

firstSen :: [Char]
firstSen = "Tyger Tyger, burning bright\n"
secondSen :: [Char]
secondSen = "In the forests of the night\n"
thirdSen :: [Char]
thirdSen = "What immortal hand or eye\n"
fourthSen :: [Char]
fourthSen = "Could frame thy fearful\
\ symmetry?"

sentences :: [Char]
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines [] = []
myLines xs =
  line : (myLines rest)
  where
    line = takeWhile (/='\n') xs
    rest = drop 1 . dropWhile (/='\n') $ xs

shouldEqual :: [String]
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?" ]

main :: IO ()
main =
    print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)
