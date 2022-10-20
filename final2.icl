module final2
import StdEnv

:: Tree a = Node a (Tree a) (Tree a) | Leaf
:: Month = January | February  | March   | April    | May | June | July
         | August  | September | October | November | December

bestTree = Node 10(Node 6(Node 1 Leaf(Node 5(Node 2 Leaf(Node 4(Node 3 Leaf Leaf)Leaf))Leaf))Leaf)(Node 14(Node 11 Leaf(Node 13(Node 12 Leaf Leaf)Leaf))(Node 17(Node 15 Leaf(Node 16 Leaf Leaf))(Node 19(Node 18 Leaf Leaf)(Node 20 Leaf Leaf))))
ourTree = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))		
noTree = Leaf
unitTree = Node 1337 Leaf Leaf

extractNode :: (Tree Int) -> Int
extractNode (Node x l r) = x

goL :: (Tree Int) -> (Tree Int)
goL (Node x l r) = l
goR :: (Tree Int) -> (Tree Int)
goR (Node x l r) = r
isLeaf :: (Tree Int) -> Bool
isLeaf Leaf = True
isLeaf _ = False

//Write a function that takes a tree as a parameter and returns a list of leaves.
// An empty tree will return [] and a single element tree will return a list of one element.

leaves :: (Tree Int) -> [Int]
leaves Leaf = []
leaves (Node x l r) 
| isLeaf l && isLeaf r = [x] 
= leaves l ++ leaves r

//Start = leaves bestTree //[3,12,16,18,20]
//Start = leaves ourTree //[1,8,11,19,24,28]
//Start = leaves unitTree //[1337]
//Start =  leaves noTree //[]

:: Q = { nom :: Int , den :: Int }

:: Beer = {name :: String, price :: Real, ratings :: [Int]}
// instances originally were not given

instance == Beer 
  where 
     (==) b1 b2 = b1.name == b2.name && b1.price == b2.price
instance < Beer 
  where 
     (<) b1 b2 = (toReal (sum b1.ratings)/toReal (length b1.ratings)) < (toReal (sum b2.ratings)/toReal (length b2.ratings))

//:: Tree a = Node a (Tree a) (Tree a) | Leaf

btree = Node 4 (Node 2 (Node 1 Leaf Leaf)(Node 3 Leaf Leaf)) (Node 6 (Node 5 Leaf Leaf)(Node 7 Leaf Leaf))

ctree = Node 1 (Node 2 (Node 8 Leaf Leaf)(Node 9 (Node 4 (Node 16 Leaf Leaf) Leaf) Leaf)) (Node 7 (Node 3 Leaf Leaf)(Node 2 Leaf Leaf))

atree = Node 4 (Node 2 (Node 1 Leaf Leaf)(Node 3 Leaf Leaf)) (Node 6 (Node 3 Leaf Leaf)(Node 7 Leaf Leaf))

:: Place = {x :: Int, y :: Int, name1 :: String}

Deak = {x = 0, y = 0, name1 = "Deak Ferenc"}

ELTE = {x = 5, y = 6, name1 = "ELTE"}

Nyugati = {x = 4, y = 2, name1 = "Nyugati Palyaudvar"}

Corvin = {x = 4, y = 3, name1 = "Corvin Negyed"}

KoKi = {x=10, y=12, name1="Kobanya Kispest"}

Keleti = {x=5, y=2, name1="Keleti Palyaudvar"}


Coors = {name="Coors", price=2.75, ratings=[2,3,2,2,1]}

Miller = {name="Miller", price=3.00, ratings=[3,2,2,3,2,2,2]}

SamAdams = {name="Samuel Adams", price=4.00, ratings=[3,3,4,2]}

Guinness = {name="Guinness", price=5.00, ratings=[2,4,4,3,5,3]}

Pabst = {name="Pabst Blue Ribbon", price=2.00, ratings=[1,1,2,1,1,2,1,1,2,3]}

BlueMoon = {name="Blue Moon", price=3.75, ratings=[4,3]}

TreeOne = (Node SamAdams (Node Coors (Node Pabst Leaf Leaf) (Node Miller Leaf Leaf)) (Node BlueMoon (Node Guinness Leaf Leaf) Leaf))

TreeTwo = (Node Miller (Node Coors Leaf Leaf) (Node SamAdams Leaf Leaf))

ListOne = [Coors, Miller, Coors, Coors, SamAdams, Guinness, Guinness, Guinness, BlueMoon]

ListTwo = [Coors, Miller, SamAdams, Guinness]

ListThree = [Pabst, Pabst, Pabst, Pabst, Pabst, Pabst, Pabst, Pabst, Pabst, Pabst, Pabst, Pabst, Pabst, Pabst, Pabst]



/** * 1. Given a list, write a function that creates sublists with number of elements equal to the first element of that sublist.
For example: [1,2,3,4,5,6,7,8] -> [[1], [2,3], [4,5,6,7], [8]] */

f1 :: [Int] -> [[Int]]
f1 [] = []
f1 list =  [take (hd list) list] ++ f1 (drop (hd list) list )



//Start = f1 [1..20] //[[1],[2,3],[4,5,6,7],[8,9,10,11,12,13,14,15],[16,17,18,19,20]]
//Start = f1 [5,2,4,3,4,1,2,5,3,6,2,6,7] //[[5,2,4,3,4],[1],[2,5],[3,6,2],[6,7]]

/** * 2. Given a tree, find the level between max node and min node. */

treeToList :: (Tree Int) -> [Int]
treeToList Leaf = []
treeToList (Node x l r) = [x] ++ treeToList l ++ treeToList r

//Start = treeToList btree

max :: Int Int -> Int
max a b
| a > b = a 
= b

depthTree :: (Tree Int) -> Int
depthTree Leaf = 0
depthTree (Node x l r) = 1 + max (depthTree l) (depthTree r)

//Start = depthTree btree 

index :: [Int] Int -> Int 
index list num = hd [ i \\ x <- list & i <- [0..] | x == num ]

//Start = index [1,2,3,6,7] 6

level :: (Tree Int) -> Int
level tree = (index list (maxList list)) - (index list (minList list)) 
where 
      list = treeToList tree 

//Start = level btree  
//Start = level ctree //4
Start = level btree//0

























