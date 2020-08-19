module Ch12.Natural where

-- As natural as any
-- competitive bodybuilder
data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat n =
  if (n >= 0)
    then Just $ go n
    else Nothing
  where
    go 0 = Zero
    go n = Succ (go (n - 1))
