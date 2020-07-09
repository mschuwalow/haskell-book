module Ch9.SquareCube where

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

myTuples = [ (x, y) |
              x <- mySqr,
              y <- myCube,
              x < 50,
              y < 50
            ]

nTuples = length myTuples
