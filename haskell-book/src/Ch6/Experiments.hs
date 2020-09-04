module Ch6.Experiments where

data TisAnInteger = TisAn Integer

fu0 :: Integer -> Bool
fu0 a = a == 0

fu :: TisAnInteger -> Bool
fu (TisAn a) = a == 0

bar :: TisAnInteger -> TisAnInteger -> Bool
bar (TisAn a) (TisAn b) = a == b

(#) :: a -> Integer
(#) _ = 0

(^) :: Integer -> Integer -> Bool
a ^ b = (==) a b

(##) :: TisAnInteger -> TisAnInteger -> Bool
(TisAn a) ## (TisAn b) = a == b

-- recap
data List a
  = Nil
  | Cons a (List a)

head' :: List a -> a
head' Nil = error "empty list"
head' (Cons a _) = a
