module Ch1To5.ChangeMood where

data Mood = Blah | Woot deriving (Show)

changeMood :: Mood -> Mood
changeMood Woot = Blah
changeMood Blah = Woot
