{-# LANGUAGE Strict #-}

module Ch27.Exercises where

-- 1
data List a
  = Nil
  | Cons a (List a)
  deriving (Show)

take' :: (Ord t, Num t) => t -> List a -> List a
take' n _ | n <= 0 = Nil
take' _ Nil = Nil
take' n (Cons x xs) =
  (Cons x (take' (n - 1) xs))

map' :: (t -> a) -> List t -> List a
map' _ Nil = Nil
map' f (Cons x xs) = (Cons (f x) (map' f xs))

repeat' :: a -> List a
repeat' x = xs where xs = (Cons x xs)

main :: IO ()
main = do
  print $ take' 10 $ map' (+ 1) (repeat' 1)

-- 4

x :: a
x = undefined

y :: [Char]
y = "blah"

main' :: IO ()
main' = do
  let y' = x `seq` y
  print (snd (x, y'))
