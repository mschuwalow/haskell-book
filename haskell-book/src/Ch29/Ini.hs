{-# LANGUAGE TupleSections #-}

module Ch29.Ini where

import Ch24.Ini
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import System.Directory (listDirectory)
import Text.Trifecta (parseFromFile)

newtype FileName = FileName String deriving (Eq, Ord, Show)

parsePath :: String -> IO ()
parsePath p = do
  files <- listDirectory p
  result <- traverse (\file -> fmap (file,) (parseFile (concat [p, "/", file]))) files
  print . show . makeLogMap $ result

makeLogMap :: [(String, Maybe Config)] -> M.Map String Config
makeLogMap = M.fromList . mapMaybe (\(k, v) -> fmap (k,) v)

parseFile :: String -> IO (Maybe Config)
parseFile path =
  case getExt path of
    "ini" -> parseFromFile parseIni path
    _ -> return Nothing

getExt :: String -> String
getExt xs = drop (length xs - 3) xs
