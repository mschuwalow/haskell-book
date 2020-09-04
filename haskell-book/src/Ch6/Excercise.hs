module Ch6.Excercise where

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn a) (TisAn b) = a == b

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  (==) (Two a b) (Two c d) = a == c && b == d

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt b) = a == b
  (==) (TisAString a) (TisAString b) = a == b
  (==) _ _ = False

data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair c d) = a == c && b == d
data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple c d) = a == c && b == d
data Which a = ThisOne a | ThatOne a
instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne b) = a == b
  (==) (ThatOne a) (ThatOne b) = a == b
  (==) _ _ = False
data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello b) = a == b
  (==) (Goodbye a) (Goodbye b) = a == b
  (==) _ _ = False
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
data List a =
    Nil
  | Cons a (List a)

head' :: List a -> a
head' Nil        = error "empty list"
head' (Cons a _) = a
