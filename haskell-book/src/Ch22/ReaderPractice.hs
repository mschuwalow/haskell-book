module Ch22.ReaderPractice where

import Data.List (find)
import Data.Maybe
import Data.Monoid

x :: [Integer]
x = [1, 2, 3]

y :: [Integer]
y = [4, 5, 6]

z :: [Integer]
z = [7, 8, 9]

lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' a xs = fmap snd $ find ((== a) . fst) xs

xs :: Maybe Integer
xs = lookup' 3 $ zip x y

ys :: Maybe Integer
ys = lookup' 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

tupled :: Applicative f => f a1 -> f a2 -> f (a1, a2)
tupled fa fb = (,) <$> fa <*> fb

x1 :: Maybe (Integer, Integer)
x1 = tupled xs ys

x2 :: Maybe (Integer, Integer)
x2 = tupled ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (zn, zn)
  where
    zn = z' n

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (> 3) <*> (< 8)

main :: IO ()
main = do
  print $
    sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(> 3), (< 8), even] 7
  print $ foldMap All $ sequA 3
  print $ sequA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(> 3), (< 8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)
