module Ch9.ComprehendThyLists where

myList :: [Integer]
myList = [x ^ 2 | x <- [1 .. 100], rem x 2 == 0]

mySecondList :: [Integer]
mySecondList = [x ^ y | x <- [1 .. 10], y <- [2, 3], x ^ y < 50]

myThirdList :: [Integer]
myThirdList = [x ^ 2 | x <- mySecondList, x < 300]

mySqr :: [Integer]
mySqr = [x ^ 2 | x <- [1 .. 5]]
