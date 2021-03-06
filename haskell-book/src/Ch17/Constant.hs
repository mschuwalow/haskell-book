module Ch17.Constant where

newtype Constant a b = Constant {getConstant :: a}
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ c = Constant (getConstant c)

instance
  Monoid a =>
  Applicative (Constant a)
  where
  pure _ = Constant mempty
  Constant (f) <*> Constant (g) = Constant $ f <> g
