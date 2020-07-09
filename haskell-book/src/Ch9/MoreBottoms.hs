module Ch9.MoreBottoms where

import Data.Bool

-- boom
x1 = take 1 $ map (+1) [undefined, 2, 3]

x2 = take 1 $ map (+1) [1, undefined, 3]

-- boom
x3 = take 2 $ map (+1) [1, undefined, 3]

-- maps chars to whether they are a vowel
itIsMystery xs =
  map (\x -> elem x "aeiou") xs

-- [1,4,9,16,25,36,49,64,81,100]
x4 = map (^2) [1..10]

-- [1, 10, 20]
x5 = map minimum [[1..10], [10..20], [20..30]] -- n.b. `minimum` is not the same function -- as the `min` that we used before

-- [15, 15, 15]
x6 = map sum [[1..5], [1..5], [1..5]]


x7 :: (a -> b) -> [a] -> [b]
x7 f xs =
  bool ((f . head $ xs) : (x7 f . tail $ xs)) [] (length xs == 0)

