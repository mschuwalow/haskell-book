module Ch9.Exercise where
eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eft :: (Ord a, Enum a) => a -> a -> [a]
eft start end =
    reverse (go start [])
    where
    go start acc
      | start > end = []
      | start == end = start:acc
      | otherwise = go (succ start) (start:acc)

eftChar :: Char -> Char -> [Char]
eftChar = eft

cut :: Eq a => [a] -> a -> [[a]]
cut x y =
  go x []
  where
    go [] acc = reverse acc
    go remainder acc =
      go next (firstword:acc)
      where
        firstword = takeWhile (/= y) $ remainder
        next = dropWhile (== y) . dropWhile (/= y) $ remainder

firstSen :: [Char]
firstSen = "Tyger Tyger, burning bright\n"
secondSen :: [Char]
secondSen = "In the forests of the night\n"
thirdSen :: [Char]
thirdSen = "What immortal hand or eye\n"
fourthSen :: [Char]
fourthSen = "Could frame thy fearful symmetry?"
sentences :: [Char]
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines sentences = cut sentences '\n'

shouldEqual :: [[Char]]
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?" ]

main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences
  == shouldEqual)

  --List comprehensions

mySqr :: [Integer]
mySqr = [x^2 | x <- [1..10]]
ex :: [Integer]
ex = [x | x <- mySqr , rem x 2 == 0]
exTwo :: [(Integer, Integer)]
exTwo = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
exThree :: [(Integer, Integer)]
exThree = take 5 [ (x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]

--Square Cube
mySrqTwo :: [Integer]
mySrqTwo = [x^2 | x <- [1..5]]
myCube :: [Integer]
myCube = [y^3 | y <- [1..5]]
qOne :: [(Integer, Integer)]
qOne = [(x, y) | x <- mySrqTwo, y <- myCube]
qTwo :: [(Integer, Integer)]
qTwo = [(x, y) | x <- mySrqTwo, y <- myCube, x < 50, y < 50]
qThree :: Int
qThree = length [(x, y) | x <- mySrqTwo, y <- myCube, x < 50, y < 50]
