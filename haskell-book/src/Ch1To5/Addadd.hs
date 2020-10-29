module Ch1To5.Addadd where

addadd :: String -> String
addadd x = x ++ "!"

fifth :: String -> String
fifth x = take 1 (drop 4 x)

dropnine :: String -> String
dropnine x = drop 9 x
