module Ch9.BottomMadness where

-- boom
x1 = [x ^ y | x <- [1 .. 5], y <- [2, undefined]]

x2 = take 1 $ [x ^ y | x <- [1 .. 5], y <- [2, undefined]]

-- boom
x3 = sum [1, undefined, 3]

x4 = length [1, 2, undefined]

-- boom
x5 = length $ [1, 2, 3] ++ undefined

x6 = take 1 $ filter even [1, 2, 3, undefined]

-- boom
x7 = take 1 $ filter even [1, 3, undefined]

x8 = take 1 $ filter odd [1, 3, undefined]

x9 = take 2 $ filter odd [1, 3, undefined]

-- boom
x10 = take 3 $ filter odd [1, 3, undefined]
