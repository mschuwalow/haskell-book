module Ch1To5.Print3broken where

greeting :: String
greeting = "yarr"

yarr :: String
yarr = "yarr"

main :: IO ()
main = do
  putStrLn greeting
  putStrLn printSecond
  where
    printSecond = "yerr"
