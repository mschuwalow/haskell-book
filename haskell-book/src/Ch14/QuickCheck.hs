{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}

module Ch14.QuickCheck where

import Data.Foldable (traverse_)
import Data.List (sort)
import Test.QuickCheck

data AnyTestable = forall s. Testable s => T String s

quickCheckAny :: AnyTestable -> IO ()
quickCheckAny (T name t) = do
  putStrLn $ "Checking property: `" ++ name ++ "`"
  quickCheck t

half :: Fractional a => a -> a
half x = x / 2

-- / porperties

prop_halfIdentity :: (Eq c, Fractional c) => c -> Bool
prop_halfIdentity x =
  x == ((* 2) . half $ x)

prop_listOrdered :: (Ord a) => [a] -> Bool
prop_listOrdered xs =
  snd . foldr go (Nothing, True) . sort $ xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, _) = (Just y, x >= y)

prop_plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
prop_plusAssociative x y z =
  x + (y + z) == (x + y) + z

prop_plusCommutative :: (Eq a, Num a) => a -> a -> Bool
prop_plusCommutative x y = x + y == y + x

prop_multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
prop_multAssociative x y z =
  x * (y * z) == (x * y) * z

prop_multCommutative :: (Eq a, Num a) => a -> a -> Bool
prop_multCommutative x y = x * y == y * x

prop_quotRem :: Integral a => a -> a -> Property
prop_quotRem x y =
  y /= 0
    ==> (quot x y) * y + (rem x y) == x

prop_divMod :: Integral a => a -> a -> Property
prop_divMod x y =
  y /= 0
    ==> (div x y) * y + (mod x y) == x

prop_expAssociative :: (Integral b1, Integral b2, Num a, Eq a) => a -> b1 -> b2 -> Bool
prop_expAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z

prop_expCommutative :: Integral b => b -> b -> Bool
prop_expCommutative x y = x ^ y == y ^ x

prop_reverseId :: Eq a => [a] -> Bool
prop_reverseId xs =
  (reverse . reverse $ xs) == xs

prop_dollarSignId :: Eq a => Fun a a -> a -> Bool
prop_dollarSignId f a =
  ((applyFun f) $ a) == applyFun f a

prop_funCompose :: Eq b1 => Fun b2 b1 -> Fun a b2 -> a -> Bool
prop_funCompose f g x =
  ((applyFun f) . (applyFun g) $ x) == applyFun f (applyFun g x)

prop_foldrConcat :: (Eq a, Foldable t) => t [a] -> Bool
prop_foldrConcat xs =
  foldr (++) [] xs == concat xs

prop_length :: Int -> [a] -> Bool
prop_length n xs =
  length (take n xs) == n

prop_readShow :: (Eq a, Read a, Show a) => a -> Bool
prop_readShow x =
  (read (show x)) == x

tests :: [AnyTestable]
tests =
  [ T "half identity" (prop_halfIdentity @Double),
    T "list ordered" (prop_listOrdered @Integer),
    T "plus - associative" (prop_plusAssociative @Integer),
    T "plus - commutative" (prop_plusCommutative @Integer),
    T "mult - associative" (prop_multAssociative @Integer),
    T "mult - commutative" (prop_multCommutative @Integer),
    T "quot rem" (prop_quotRem @Integer),
    T "div mod" (prop_divMod @Integer),
    --, T "exp - associative"         (prop_expAssociative @Integer @Integer @Integer)
    --, T "exp - commutative"         (prop_expCommutative @Integer)
    T "reverse twise is identity" (prop_reverseId @Integer),
    T "dollar sign identity" (prop_dollarSignId @Integer),
    T "compose functions with ." (prop_funCompose @Integer @Integer @Integer),
    T "foldr - concat relation" (prop_foldrConcat @Integer @[]),
    --, T "length - take"             (prop_length @Integer)
    T "read - show" (prop_readShow @Integer)
  ]

runQc :: IO ()
runQc =
  traverse_ quickCheckAny tests
