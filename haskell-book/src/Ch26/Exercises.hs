module Ch26.Exercises where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Functor.Identity

rDec :: Num a => Reader a a
rDec = asks (subtract 1)

rShow :: Show a => ReaderT a Identity String
rShow = asks show

printAndInc :: (Num a, Show a) => ReaderT a IO a
printAndInc = do
  n <- ask
  lift $ putStrLn (mconcat ["Hi: ", show n])
  return $ n + 1

sPrintIncAccum ::
  (Num a, Show a) =>
  StateT a IO String
sPrintIncAccum = do
  n <- get
  let s = show n
  lift $ putStrLn (mconcat ["Hi: ", s])
  put (n + 1)
  return s

-- fix the code

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e ->
      putStrLn ("Good, was very excite: " ++ e)
