module Ch9.SquareCube where

mySqr :: [Integer]
mySqr = [x ^ 2 | x <- [1 .. 5]]

myCube :: [Integer]
myCube = [y ^ 3 | y <- [1 .. 5]]

myTuples :: [(Integer, Integer)]
myTuples =
  [ (x, y)
    | x <- mySqr,
      y <- myCube,
      x < 50,
      y < 50
  ]

nTuples :: Int
nTuples = length myTuples
