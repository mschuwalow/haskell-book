module Ch22.Reader where

newtype Reader r a
  = Reader {runReader :: r -> a}

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure a = Reader $ const a
  (Reader f) <*> (Reader fa) = Reader $ \r -> (f r) (fa r)

instance Monad (Reader r) where
  (Reader fa) >>= f = Reader $ \r -> runReader (f (fa r)) r

ask :: Reader a a
ask = Reader id

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader

newtype HumanName
  = HumanName String
  deriving (Eq, Show)

newtype DogName
  = DogName String
  deriving (Eq, Show)

newtype Address
  = Address String
  deriving (Eq, Show)

data Person
  = Person
      { humanName :: HumanName,
        dogName :: DogName,
        address :: Address
      }
  deriving (Eq, Show)

data Dog
  = Dog
      { dogsName :: DogName,
        dogsAddress :: Address
      }
  deriving (Eq, Show)

getDogM :: Reader Person Dog
getDogM = do
  name <- asks dogName
  addy <- asks address
  return $ Dog name addy
