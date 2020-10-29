-- FumctionWithWhere.hs
module Ch1To5.FunctionWithWhere where

printInc :: (Show a, Num a) => a -> IO ()
printInc n = print plusTwo
  where
    plusTwo = n + 2
