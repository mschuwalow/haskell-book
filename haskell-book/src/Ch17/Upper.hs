module Ch17.Upper where

f1 :: Maybe [Char]
f1 = const <$> Just "Hello" <*> pure "World"

f2 :: Maybe (Integer, Integer, [Char], [Integer])
f2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
