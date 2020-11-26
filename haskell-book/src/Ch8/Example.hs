module Ch8.Example where

-- bad
listUntil :: Int -> [Int]
listUntil n =
  go 0 []
  where
    go x acc
      | x > n = acc
      | otherwise = go (x + 1) (acc ++ [x])

listUntil' :: Int -> [Int]
listUntil' n =
  go n []
  where
    go x acc
      | x < 0 = acc
      | otherwise = go (x - 1) (x : acc)
