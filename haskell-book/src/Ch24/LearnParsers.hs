module Ch24.LearnParsers where

import Data.Foldable
import Data.Functor
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser b
one' = one >> stop

one'' :: Parser Char
one'' = one >> eof $> '1'

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser b
oneTwo' = oneTwo >> stop

oneTwo'' :: Parser Char
oneTwo'' = oneTwo >> eof $> '2'

p123 :: String -> Result String
p123 = parseString parser mempty
  where
    parser = do
      n <- (show <$> integer)
      eof
      return n

string' :: (Monad f, CharParsing f) => [Char] -> f [Char]
string' s =
  try (traverse_ char s) $> s

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL :: [Char] -> IO ()
pNL s =
  putStrLn $ '\n' : s

main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "one'':"
  testParse one''
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneTwo'':"
  testParse oneTwo''
