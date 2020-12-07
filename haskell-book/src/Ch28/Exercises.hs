module Ch28.Exercises where

newtype DList a = DL {unDL :: [a] -> [a]}

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton a = DL (a :)
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList dl = unDL dl []
{-# INLINE toList #-}

-- Prepend a single element to a dlist.
infixr 9 `cons`

cons :: a -> DList a -> DList a
cons x xs = DL ((x :) . unDL xs)
{-# INLINE cons #-}

-- Append a single element to a dlist.
infixl 9 `snoc`

snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (x :))
{-# INLINE snoc #-}

-- Append dlists.
append :: DList a -> DList a -> DList a
append a b = DL (unDL a . unDL b)
{-# INLINE append #-}

-- Queue

data Queue a = Queue
  { enqueue :: [a],
    dequeue :: [a]
  }
  deriving (Eq, Show)

-- adds an item
push :: a -> Queue a -> Queue a
push a q = q {enqueue = a : enqueue q}

pop :: Queue a -> Maybe (a, Queue a)
pop Queue {dequeue = [], enqueue = []} = Nothing
pop q@Queue {dequeue = (x : xs)} = Just (x, q {dequeue = xs})
pop Queue {dequeue = [], enqueue = xs} = Just (y, Queue {dequeue = ys, enqueue = []})
  where
    (y, ys) = case reverse xs of
      [] -> error "impossible"
      (y : ys) -> (y, ys)
