module Deeppt_g3

import StdEnv

/*

    Task 1:
    Update a tree values with the given function.

    E.G:

        07

       /   \

     02     20

     /\     / \

    01 04  10 30

TASK: For each node, decide whether the value at that node is odd or not.
    so result:
        True
      /     \
  False     False
    /\      /     \
True False False  False

       

*/




:: Tree a = Node a (Tree a) (Tree a) | Leaf
tree1 = Node 7
                        ( Node 2 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf))
                        ( Node 20 (Node 10 Leaf Leaf) (Node 30 Leaf Leaf))

tree2 = Node 5
                        ( Node 3 (Node 13 Leaf Leaf) (Node 11 Leaf Leaf))
                        ( Node 1 (Node 7 Leaf Leaf) (Node 9 Leaf Leaf))




check :: (Tree Int) -> (Tree Bool)
check Leaf = Leaf
check (Node x l r) = Node (isOdd x) (check l) (check r)


//Start = check tree1
            /*          
                        Node True
                        ( Node False (Node True  Leaf Leaf) (Node False Leaf Leaf))
                        ( Node False (Node False Leaf Leaf) (Node False Leaf Leaf)
            */
//Start = check tree2
            /*          
                        All are True
            */

   

/*-2
Write a function which returns the number of times the first string parameter contains
   the second string parameter.
*/

aux3 :: String -> [Char]
aux3 str = [ x \\ x <-: str]

index :: [Char] Char -> Int
index [] c = 0
index [x:xs] c 
| x == c = 0
= 1 + index xs c

allIndex :: [Char] [Char] -> [Int]
allIndex a b = [ index a x \\ x <- b ]

isSub2 a b = [ x \\ x <-: b | isMember x (aux3 a) ]

isSub :: String String -> [Bool]
isSub a b 
| length f1 < 1 = [allIndex (aux3 a) (isSub2 a b) == allIndex (aux3 b) (aux3 b)]
| length f2 < 1 = [allIndex (aux3 a) (isSub2 a b) == allIndex (aux3 b) (aux3 b)]
= [ (x > y || x == y) && (f3 == f4) \\ x <- f1 & y <- f2 ]
where 
     f1 = allIndex (aux3 a) (isSub2 a b)
     f2 = allIndex (aux3 b) (aux3 b)
     f3 = (aux3 b)  
     f4 = (isSub2 a b)
 

num_times :: String String -> Int
num_times str1 str2 = length (filter ((==)True)(isSub str1 str2))

 
//Start = num_times "abcdhdkglabcldfkdabc" "abc" // 3  
//Start = num_times "Hello, hi, hey" "Hi!!" // 0
//Start = num_times "ab c ab ab de ab fg" "ab" // 4 











