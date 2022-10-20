
module pt_g3
import StdEnv


/*
Write binary search tree ADT. It should have two types: Node with value, and left/right subtrees and Leaves.
Write treeToFilteredList function which takes two arguments, a map function and a tree. The function should
return a list of mapped values from the given tree that are Even. Simply, map all the tree values then
return the even ones only.
*/



// TODO
:: Tree a = Node a (Tree a) (Tree a) | Leaf



tree1 :: Tree Int
tree1 = (Node 4 (Node 10 (Node 6 Leaf Leaf)(Node 11 Leaf Leaf)) (Node 20 (Node 12 Leaf Leaf) Leaf))



tree2 :: Tree Int
tree2 = (Node 5 (Node 10 (Node 31 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 17 (Node 31 (Node 14 (Node 12 Leaf Leaf) Leaf) Leaf) (Node 11 Leaf Leaf) ))



tree3 :: Tree Int
tree3 = (Node 12 (Node 11 (Node 11 (Node 32 Leaf Leaf) Leaf) Leaf) (Node 4 (Node 17 (Node 5 (Node 7 Leaf Leaf) Leaf) Leaf) (Node 3 Leaf (Node 4 Leaf Leaf)) ))



tree4 :: Tree Int
tree4 = (Node 7 (Node 11 tree1 tree2) (Node 5 tree3 tree2))



tree5 :: Tree Int
tree5 = Node 1 tree3 tree4


treeToList :: (Tree a) -> [a]
treeToList Leaf = []
treeToList (Node x l r) = treeToList l ++ [x] ++ treeToList r


// TODO
treeToFilteredList:: (a -> a) (Tree a) -> [a] | isEven a
treeToFilteredList fun tree = map fun (treeToList tree) 


// NOTE::  Order Does not matter For output results
//Start = treeToFilteredList (\x = (x rem 2)) tree1 // [0,0,0,0,0]							
// Start = treeToFilteredList (\x = x*3) tree2 // [30,36,42]									
//Start = treeToFilteredList (\x = x+x) tree3 // [64,22,22,24,14,10,34,8,6,8]
// Start = treeToFilteredList (\x = x) tree4 // [6,10,4,12,20,10,12,14,32,12,4,4,10,12,14]
Start = treeToFilteredList (\x = x*x*x) tree5 // [32768,1728,64,64,216,1000,64,1728,8000,1000,1728,2744,32768,1728,64,64,1000,1728,2744]








