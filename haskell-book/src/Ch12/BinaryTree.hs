module Ch12.BinaryTree where

import Ch11.BinaryTree

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f initial =
  go initial
  where
    go =
      maybe Leaf (\(a1, b, a2) -> Node (go a1) b (go a2))
      . f

treeBuild :: Integer -> BinaryTree Integer
treeBuild n =
  unfold (\x -> if (x < n) then Just (x + 1, x, x + 1) else Nothing) 0
