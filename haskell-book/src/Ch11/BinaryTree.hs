module Ch11.BinaryTree where

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Ord, Eq, Show)

insert' ::
  Ord a =>
  a ->
  BinaryTree a ->
  BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a    = Node left a right
  | b < a     = Node (insert' b left) a right
  | otherwise = Node left a (insert' b right)

mapTree ::
  (a -> b) ->
  BinaryTree a ->
  BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node
    (Node Leaf 3 Leaf)
    1
    (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected =
  Node
    (Node Leaf 4 Leaf)
    2
    (Node Leaf 5 Leaf)

-- acceptance test for mapTree
testMapTree :: IO ()
testMapTree =
  if mapTree (+ 1) testTree' == mapExpected
    then putStrLn "mapTree - passed!"
    else putStrLn "mapTree - failed!"

-- tree traversals
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

testTree :: BinaryTree Integer
testTree =
  Node
    (Node Leaf 1 Leaf)
    2
    (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
    then putStrLn "preorder - passed!"
    else putStrLn "preorder - failed!"

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
    then putStrLn "inorder - passed!"
    else putStrLn "inorder - failed!"

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
    then putStrLn "postorder - passed!"
    else putStrLn "postorder - failed!"

main :: IO ()
main = do
  testMapTree
  testPreorder
  testInorder
  testPostorder
