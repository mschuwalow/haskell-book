module Sing where

fstString :: [Char] ++ [Char]
fstString x = x ++ "in the rain"

sndString :: [Char] -> Char
sndString y = y ++ "over the rainbow"

  Sing (x, y) =
   if (x > y) then fstString x or sndString y
      where
        fstString = "Singin"
        sndString = "somewhere"
