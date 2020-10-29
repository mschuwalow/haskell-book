module Ch1To5.Hwoo where

f1 :: Integer
f1 = x * 3 + y
  where
    x = 3
    y = 1000

f2 :: Integer
f2 = x * 5
  where
    y = 10
    x = 10 * 5 + y

f3 :: Double
f3 = z / x + y
  where
    x = 7
    y = negate x
    z = y * 10
