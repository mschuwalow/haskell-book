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
