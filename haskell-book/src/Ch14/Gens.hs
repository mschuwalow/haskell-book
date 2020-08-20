module Ch14.Gens where

import Test.QuickCheck

data Fool
  = Fulse
  | Frue
  deriving (Eq, Show)

-- equal chances
genFoolEqual :: Gen Fool
genFoolEqual =
  oneof [return Fulse, return Frue]

-- 2/3 chance of Fulse, 1/3 chance of Frue
genFoolFulse :: Gen Fool
genFoolFulse =
  frequency
    [ (2, return Fulse),
      (1, return Frue)
    ]
