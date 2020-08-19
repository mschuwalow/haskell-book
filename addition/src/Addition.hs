module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom =
  go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

multByAddition :: (Num a, Ord a) => a -> a -> a
multByAddition a b
  | b < 0 = - (go (- b) 0)
  | otherwise = go b 0
  where
    go 0 acc = acc
    go n acc = go (n - 1) (acc + a)

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
  describe "Division" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)
  describe "MultByDivision" $ do
    it "1 multiplied by 1 is 1" $ do
      multByAddition 1 1 `shouldBe` 1
    it "1 multiplied by 0 is 0" $ do
      multByAddition 1 0 `shouldBe` 0
    it "2 multiplied by -2 is -4" $ do
      multByAddition 2 (-2) `shouldBe` (-4)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
