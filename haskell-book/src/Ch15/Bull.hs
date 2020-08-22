module Ch15.Bull where

import Ch15.Laws
import Test.QuickCheck

data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency
      [ (1, return Fools),
        (1, return Twoo)
      ]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: Bull -> Bull -> Bull -> Bool)
  quickCheck (monoidLeftIdentity :: Bull -> Bool)
  quickCheck (monoidRightIdentity :: Bull -> Bool)
