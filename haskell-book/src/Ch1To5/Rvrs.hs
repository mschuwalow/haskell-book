module Ch1To5.Rvrs where

rvrs :: String -> String
rvrs x =
  awesome ++ " " ++ is ++ " " ++ curry
  where
    curry = take 5 x
    is = take 2 . drop 6 $ x
    awesome = take 7 . drop 9 $ x
