module BinaryTree where

data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Show, Eq, Ord)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+ 1) testTree' == mapExpected
    then print "yup okay!!"
    else error "test failed!"

--
-- Convert binary trees to lists
--
testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : (preorder left ++ preorder right)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder passed"
    else putStrLn "Preorder failed"

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder passed"
    else putStrLn "Inorder failed"

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
    then putStrLn "postorder passed"
    else putStrLn "postorder failed"

-- any traversal order is fine
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f acc Leaf = acc
foldTree f acc (Node left a right) =
  let accLeft = foldTree f acc left
   in let accRight = foldTree f accLeft right
       in f a accRight

testFoldTree :: IO ()
testFoldTree =
  if foldTree (+) 0 testTree == 6
    then putStrLn "foldTree passed"
    else putStrLn "foldTree failed"

testFoldTree2 :: IO ()
testFoldTree2 =
  if foldTree (-) 0 testTree == 0
    then putStrLn "foldTree2 passed"
    else putStrLn "foldTree2 failed"
