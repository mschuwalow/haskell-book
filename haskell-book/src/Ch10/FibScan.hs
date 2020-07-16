module Ch10.FibScan where

-- Scans exercises p.378

--1.
--lazyFib :: [Int]
--lazyFib = 0:1:zipWith (+) lazyFib (tail lazyFib)

fibs = 1 : scanl (+) 1 fibs
fibs20 = take 20 fibs
-- [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]


--2.
fibs100 = takeWhile (<100) fibs
-- fibs100  ->  [1,1,2,3,5,8,13,21,34,55,89]

--3.
factorialScan n = (scanl (*) 1 [1..n]) !! n
--Amin
factorial n = (scanl (flip (*)) 1 . take n )[1..]
-- factorialScan 10  ->  3628800


{-

scanl (+) 1 [1..3]
[1, 1 + 1, (1 + 1) + 2, ((1 + 1) + 2) + 3]
[1,2,4,7]


fibs :: Num a => [a]
fibs = 1 : scanl (+) 1 fibs

fibs = 1 : scanl (+) 1 []
       1 : [1]
       [1,1]

fibs = 1 : scanl (+) 1 [1,1]
       1 : [1,1+1,(1+1)+1]
       [1,1,2,3]

fibs = 1 : scanl (+) 1 [1,1,2,3]
       1 : [1,1+1,(1+1)+1,((1+1)+1)+2,(((1+1)+1)+2)+3)]
       [1,1,2,3,5,8]

fibs' = 1 : scanl (+) 0 fibs'
      = [1,0,1,1,2,3,5...

-}

