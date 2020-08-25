{-# LANGUAGE DerivingVia #-}

module Ch17.ZipList where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype ZipList a
  = ZipList [a]
  deriving (Eq, Show)
  deriving (Functor) via []
  deriving (Arbitrary, Semigroup, Monoid) via [a]

instance
  Eq a =>
  EqProp (ZipList a)
  where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList l) = xs
         in take 3000 l
      ys' =
        let (ZipList l) = ys
         in take 3000 l

instance Applicative ZipList where
  pure a = ZipList $ repeat a
  (ZipList a) <*> (ZipList b) = ZipList $ go a b
    where
      go [] _ = []
      go _ [] = []
      go (f : fs) (x : xs) = (f x) : (go fs xs)

runQc :: IO ()
runQc = do
  quickBatch $ applicative (ZipList [("", "", "")])
