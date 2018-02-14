module BinaryTree where

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a 
        => a 
        -> BinaryTree a 
        -> BinaryTree a
insert' b Leaf = 
    Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

mapTree :: (a -> b)
    -> BinaryTree a
    -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
    Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf)
        1 
            (Node Leaf 4 Leaf)

mapExpected =
    Node (Node Leaf 4 Leaf)
        2 (
            Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
    if mapTree (+1) testTree' == mapExpected
        then print "yup okay!"
        else error "test failed!"


-- Converting BinaryTree to list
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node l v r) = [v] ++ (preorder l) ++ (preorder r)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node l v r) = (inorder l) ++ [v] ++ (inorder r)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node l v r) = (inorder l) ++ (inorder r) ++ [v]

-- Folding
--
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node l a r) = total where
                                b' = f a b
                                b'' = foldTree f b' l
                                total = foldTree f b'' r

testTree :: BinaryTree Integer
testTree =
    Node (Node Leaf 1 Leaf)
        2 (
            Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"


testFoldTree :: IO()
testFoldTree =
    if (foldTree (+) 0 testTree) == 6
    then putStrLn "fold fine!"
    else putStrLn "fold failed check"

-- main :: IO ()
main = do
    testPreorder
    testInorder
    testPostorder
    testFoldTree

