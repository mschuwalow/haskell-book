module Ch13.Person where

import Data.Either.Combinators (mapLeft)
import Text.Read (readEither)

type Name = String

type Age = Integer

data Person
  = Person Name Age
  deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Enter your name: "
  name <- getLine
  putStrLn "Enter your age: "
  age <- getLine
  case parsePerson name age of
    Right x ->
      putStrLn $ "Yay! Successfully got a person: " ++ (show x)
    Left e ->
      putStrLn $ "Whoops: " ++ (show e)
  where
    parsePerson name age = do
      age' <- mapLeft (\_ -> PersonInvalidUnknown "Invalid age") $ readEither age
      mkPerson name (read age')
