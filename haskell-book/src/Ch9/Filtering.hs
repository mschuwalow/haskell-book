module Ch9.Filtering where

x1 xs =
  filter (\x -> mod x 3 == 0) xs

x2 = length . x1

x3 = filter (not . flip elem ["the", "a", "an"]) $ words "the brown dog was a goof"
