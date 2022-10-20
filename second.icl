module second
import StdEnv

/*
:: Student = {name :: String
,id :: String
,grades :: {Int}}



/*
* Write a function oddStudents that takes array of students, and returns the names of the oddStudents.
* A student is odd if
* the sum of their grades is smaller than 100 and is an odd number.
*/



oddStudents :: {Student} -> [String]
oddStudents arr = [a.name \\ a<-:arr |sum [b \\ b<-: a.grades] < 100 && isOdd (sum [b \\ b<-: a.grades]) ]



// Intended for tests. Do not remove!
student1 = {name="a",id="st1",grades={20,40,13}}
student2 = {name="b",id="st2",grades={50,13,10,42}}
student3 = {name="c",id="st3",grades={13,70}}
student4 = {name="d",id="st4",grades={}}



// Start = oddStudents {} // []
// Start = oddStudents {student1} // ["a"]
//Start = oddStudents {student1, student2, student3, student4} // ["a","c"]
//Start = oddStudents {student4} // []
*/
:: Tree a = Node a (Tree a) (Tree a) | Leaf
// Node x leftChild rightChild
/*
[1,2,3,4]
log2(n)
(n*logn)



[a,b,c,d,e,f,g..l]



k
/ \
j l
*/



/*
8
/ \
6 12
/ \ / \
3 7 10 L
L 5 LL LL
L L
*/

smallTree :: (Tree Int)
smallTree = Node 8 (Node 6 Leaf Leaf) (Node 12 Leaf Leaf)


//Getting the value at the node
extractNode :: (Tree a) -> a
extractNode (Node x l r) = x



//Going down left/right subtree
goL :: (Tree a) -> (Tree a)
goL (Node x l r) = l



goR :: (Tree a) -> (Tree a)
goR (Node x l r) = r



// Start = goR ourTree
// Start = goL ourTree




//Start = l == Leaf
//Checking if we're at a leaf



max :: Int Int -> Int
max a b
| a < b = b
= a




isLeaf :: (Tree a) -> Bool
isLeaf Leaf = True
isLeaf _ = False



//Get a list of subtrees from a node.
getSubTrees :: (Tree a) -> [(Tree a)]
getSubTrees (Node x l r) = [l,r]



//Start = map (\(Node x l r) = x) (getSubTrees smallTree)



//[Node 6 Leaf Leaf ,Node 12 Leaf Leaf]



// 8 level 1
// 6 12 level 2
// 4 7 11 13 level 3
// L L L L 4 level 4
// Leaf Leaf



//[8,6,12,4,7,11,13]



//[6,8,12]
// preOrder Of this ^ [8,6,4,7,12,11,13]
 



treeToList :: (Tree a) -> [a]
treeToList Leaf = []
treeToList (Node x l r) = treeToList l ++ [x] ++ treeToList r



//Start = treeToList smallTree //[6,8,12]
//Start = treeToList ourTree


minTreeNotBST :: (Tree a) -> a | Ord a
minTreeNotBST tree = minList (treeToList tree)



//Get the min value of a Tree
minTree :: (Tree a) -> a | Ord a
minTree (Node x l r)
| isLeaf l = x
= minTree l



//Start = minTree ourTree




//Reverse a tree

ourTree :: (Tree Int)
ourTree = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))

reverseTree :: (Tree a) -> (Tree a)
reverseTree Leaf = Leaf
reverseTree (Node x l r) = (Node x (reverseTree r) (reverseTree l))




//Start = reverseTree ourTree



//Get the max value of a BST
maxTree :: (Tree a) -> a | Ord a
maxTree tree = minTree(reverseTree tree)
//Start = maxTree ourTree



maxTreeNotBST :: (Tree a) -> a | Ord a
maxTreeNotBST tree = maxList (treeToList tree)









///
:: BinTree a = BinNode a (BinTree a) (BinTree a) | TreeEnd
bt1 = (BinNode 3 (BinNode 2 TreeEnd (BinNode 4 TreeEnd TreeEnd)) (BinNode 1 (BinNode 5 TreeEnd TreeEnd) TreeEnd))
bt2 = (BinNode 0 TreeEnd TreeEnd)
bt3 = (BinNode 10 (BinNode 11 bt1 bt2) (BinNode 12 TreeEnd bt2))
///


//Create a list of all subtrees


subTreeList :: (BinTree a) -> [(BinTree a)]
subTreeList TreeEnd = []
subTreeList (Node x l r) = subTreeList(l) ++ [(Node x l r)] ++ subTreeList (r)

Start = subTreeList bt1

// subTreeList :: (Tree a) -> [(Tree a)]
// subTreeList Leaf = []
// subTreeList (Node x l r) = subTreeList(l) ++ [(Node x l r)] ++ subTreeList(r)



//Start = subTreeList ourTree
//Start = treeToList ourTree



//Extract sublists countaining a specific element



//extractSubLists :: a (Tree a) -> [(Tree a)] | Eq a
//extractSubLists n tree = [subtree\\subtree<-(subTreeList tree)|(extractNode subtree)==n]




// Start = extractSubLists 3 ourTree



//Get a list of children of a node
/*
getChildren n (Node x l r)
|isLeaf l && isLeaf r = []
|isLeaf l = [extractNode r]
|isLeaf r = [extractNode l]
|n == x = [extractNode l] ++ [extractNode r]
= getChildren n l ++ getChildren n r
*/
/*
getChildren :: a (Tree a) -> [a] | Eq a
getChildren n tree
| isLeaf(goL subtree) && isLeaf(goR subtree)=[]
| isLeaf(goL subtree) = [extractNode(goR subtree)]
| isLeaf(goR subtree) = [extractNode(goL subtree)]
= [extractNode(goL subtree)]++[extractNode(goR subtree)]
where
     subtree = hd(extractSubLists n tree)
*/


//Start = getChildren 20 ourTree
//Start = getChildren ((getChildren 20 ourTree)!!0) ourTree ++ getChildren ((getChildren 20 ourTree)!!1) ourTree



/*
Leaf = Node 3 Leaf Leaf
3
5
1 6
l 3 l 8
l l l 9
l l
*/
//binary search tree



// Graphs > Tree > Binary Search Trees
// Tree: Connected, Acylic graph
// B Trees, B+ trees, B- trees, AVL trees, Rose trees
// Binary Trees > Binary Search Tree



// Binary Search Tree:




SumTree :: (Tree Int) -> Int
SumTree Leaf = 0
SumTree (Node x l r) = x + (SumTree l) + (SumTree r)



getMax :: (Tree a) -> a
getMax (Node x l r)
| isLeaf r = x
= getMax r



// Start = getMax (Node 1 (Node 0 (Node -1 Leaf Leaf) Leaf) (Node 5 Leaf Leaf)) // 5



// Add a new node to a BST
addNode :: a (Tree a) -> (Tree a) | Ord, Eq a
addNode n Leaf = Node n Leaf Leaf
addNode n (Node x l r)
| n == x = (Node x l r)
| n < x = addNode n l
| n > x = addNode n r



// [2,41,2,31,3,1,2,20]



//[5,2,4,3,1]



//add 5 (add 2( add 4 (add 3 (add 1 (Leaf))) ) )



/*
1
Leaf 3
2 4
Leaf Leaf Leaf 5
Leaf Leaf
*/
listToBST :: [Int] -> (Tree Int)
listToBST [] = Leaf
listToBST [x:xs] = addNode x (listToBST xs)



// Start = listToBST [2,41,2,31,3,1,2,20]




bestTree :: (Tree Int)
bestTree = Node 10(Node 6(Node 1 Leaf(Node 5(Node 2 Leaf(Node 4(Node 3 Leaf Leaf)Leaf))Leaf))Leaf)(Node 14(Node 11 Leaf(Node 13(Node 12 Leaf Leaf)Leaf))(Node 17(Node 15 Leaf(Node 16 Leaf Leaf))(Node 19(Node 18 Leaf Leaf)(Node 20 Leaf Leaf))))
shortTree :: (Tree Int)
shortTree = Node 14(Node 11 Leaf(Node 13 Leaf Leaf))(Node 17(Node 15 Leaf Leaf)Leaf)
noTree :: (Tree Int)
noTree = Leaf
unitTree :: (Tree Int)
unitTree = Node 1337 Leaf Leaf



//Given a tree, find its depth



depthOfTree::(Tree a)->Int
depthOfTree Leaf = 0
depthOfTree (Node x l r) = 1 + max (depthOfTree l) (depthOfTree r)



//Start = depthOfTree bestTree // 7
//Start = depthOfTree ourTree // 5
//Start = depthOfTree shortTree // 3
//Start = depthOfTree unitTree // 1
//Start = depthOfTree noTree



/*
Given a Tree with nodes of type Person,
return the number of people who are older than 18.
That is, people born on or before 2003.11.10
*/
::Person = { name::String
           ,birthday::(Int,Int,Int)
            }



t1::Tree Person
t1 = Node {name = "hh", birthday = (2001,11,22)} Leaf Leaf
t2::Tree Person
t2 = Node {name = "hh", birthday = (2001,11,22)} (Node {name = "hr", birthday = (2001,11,21)} Leaf Leaf)(Node {name = "ht", birthday = (2001,11,23)} Leaf Leaf)
t3::Tree Person
t3 = Node {name = "hh", birthday = (2001,11,22)} (Node {name = "hr", birthday = (2001,11,21)} (Node {name = "hh", birthday = (2002,11,22)} Leaf Leaf) (Node {name = "hh", birthday = (1998,11,22)} Leaf Leaf))(Node {name = "ht", birthday = (2001,11,23)} Leaf Leaf)



is18 :: (Int,Int,Int) -> Bool
is18 (yr,mo,da)
| 2021 - yr > 18 = True
| 2021 - yr == 18 && mo < 11 = True
| 2021 - yr == 18 && mo == 11 && mo <= 10 = True
= False



f2 :: (Tree Person) -> Int
f2 Leaf = 0
f2 (Node x l r)
| is18 x.birthday = 1 + f2 l + f2 r
= f2 l + f2 r




//Start = f2 t2 // 3
//Start = f2 t3 // 5



/*
Write a function that takes a tree as a parameter
and returns a list of all of the numbers of the nodes whose node children count is odd
*/



childrenCount :: (Tree a) -> Int
childrenCount Leaf = 0
childrenCount (Node x l r) = 1 + childrenCount l + childrenCount r




oddKids :: (Tree Int) -> [Int]
oddKids Leaf = []
oddKids (Node x l r)
| isOdd (childrenCount l + childrenCount r) = [x] ++ oddKids l ++ oddKids r
=oddKids l ++ oddKids r




//Start = oddKids bestTree //[6,5,4,14,13,17,15]
//Start = oddKids ourTree //[7,13,18,21]
//Start = oddKids shortTree //[11,17]
//Start = oddKids unitTree //[]
//Start = oddKids noTree //[]