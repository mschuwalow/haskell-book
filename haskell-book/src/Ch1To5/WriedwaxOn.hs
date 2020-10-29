module Ch1To5.WriedwaxOn where

waxOn :: Integer
waxOn = x * 5
  where
    z = 7
    x = y ^ 2
    y = z + 8

triple :: Integer -> Integer
triple = (* 3)

waxOff :: Integer -> Integer
waxOff x = triple x
