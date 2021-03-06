{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ch11.LogicGoats where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, _) = tooMany n

instance TooMany (Int, Int) where
  tooMany (n, m) = tooMany $ n + m

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, m) = tooMany $ n + m

newtype Goats
  = Goats Int
  deriving (Eq, Show, TooMany)
