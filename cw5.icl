module cw5
import StdEnv

/*
1. Given a tree and an integer n, find the nodes equal to n and replace by '0'
*/

atree = Node 4 (Node 2 (Node 1 Leaf Leaf)(Node 3 Leaf Leaf)) (Node 6 (Node 3 Leaf Leaf)(Node 7 Leaf Leaf))


:: Tree a = Node a (Tree a) (Tree a)
          | Leaf

f1 :: Int (Tree Int) -> (Tree Int) 
f1 num Leaf = Leaf  
f1 num (Node x l r) 
| x ==  num = Node 0 (f1 num l) (f1 num r)
= Node x (f1 num l) (f1 num r)

 

//Start = f1 3 atree  //(Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 0 Leaf Leaf)) (Node 6 (Node 0 Leaf Leaf) (Node 7 Leaf Leaf)))

/*
2. Given a tree, find the level between max node and min node
*/

btree = Node 4 (Node 2 (Node 1 Leaf Leaf)(Node 3 Leaf Leaf)) (Node 6 (Node 5 Leaf Leaf)(Node 7 Leaf Leaf))

ctree =  Node 4 (Node 2 (Node 8 Leaf Leaf)(Node 9 (Node 4 (Node 16 Leaf Leaf) Leaf) Leaf)) (Node 7 (Node 3 Leaf Leaf)(Node 2 Leaf Leaf))

isLeaf :: (Tree a) -> Bool
isLeaf Leaf = True
isLeaf _ = False

minTree :: (Tree a) -> a | Ord a
minTree (Node x l r)
| isLeaf l = x
= minTree l

maxTree :: (Tree a) -> a | Ord a
maxTree tree = minTree(reverseTree tree)

reverseTree :: (Tree a) -> (Tree a)
reverseTree Leaf = Leaf
reverseTree (Node x l r) = (Node x (reverseTree r) (reverseTree l))

 
//f2 :: (Tree Int) ->Int

//Start = f2 ctree //3


/*
3. 
Define algebraic type : Day (Mon,Tue,Wed,Thu,Fri,Sat,Sun).
And define function IsWeekend :: Day -> Bool to check if it is Sat or Sun.
if it is weekend, then output "Happy day!",Otherwise,"Oh my god"
*/

instance == Day
where
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False
    
:: Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

IsWeekend :: Day -> Bool
IsWeekend day
| day == Sat || day == Sun = True
= False

//Start = IsWeekend Sat

f3 :: Day -> String
f3 day 
| IsWeekend day = "Happy day!"
= abort "Oh my god"

//Start = f3 Sun  // "Happy day!"
//Start = f3 Tue  // "Oh my god"





