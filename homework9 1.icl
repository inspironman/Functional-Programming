module homework9
import StdEnv





/*
 * Complete the function familyTree below that takes a tree of Person type,
 * and returns a string consisting of the family from left to right order.
 * treeOrgin below should return the following string:
 * "Priest(Hasbulla(Beth)(John))(Elen(Dembo)(Youth))"
*/


:: Person = {name::String, age :: Int}
 
:: Tree a = Node a (Tree a) (Tree a) | Leaf

priest :: Person
priest = {name="Priest",age=45}

grandFather :: Person
grandFather = {name="Hasbulla",age=78}

grandMother :: Person
grandMother = {name="Beth",age=76}
							
dad :: Person
dad = {name="John",age=34}

mom :: Person
mom = {name="Elen",age=36}

son :: Person
son = {name="Dembo",age=16}


daughter :: Person
daughter = {name="Youth",age=12}

treeOrgin :: (Tree Person)
treeOrgin = Node priest (Node grandFather (Node grandMother Leaf Leaf) (Node dad Leaf Leaf)) (Node mom (Node son Leaf Leaf) (Node daughter Leaf Leaf))
treeSmall :: (Tree Person)
treeSmall = Node grandFather Leaf Leaf
treeNone  :: (Tree Person)
treeNone  = Leaf

//Start = grandFather.name


//treeToList :: (Tree Person) -> String

FamilyTree :: (Tree Person) -> String
FamilyTree Leaf = "()"
FamilyTree (Node x Leaf Leaf) = x.name +++ "()()" 
FamilyTree (Node x l r) = x.name +++ "(" +++(Yeah l) +++")("+++ (Yeah r)+++")"
where
	Yeah (Node x Leaf Leaf) = x.name
	Yeah (Node x lr rt) = x.name +++ "(" +++ Yeah lr+++ ")" +++ "(" +++ Yeah rt +++ ")" 

//Start = FamilyTree treeOrgin // "Priest(Hasbulla(Beth)(John))(Elen(Dembo)(Youth))"
                              //  "Priest(Hasbulla(Beth)(John))(Elen(Dembo)(Youth))"     
//Start = FamilyTree treeSmall // "Hasbulla()()"
//Start = FamilyTree Leaf // "()"







/* Write a function that takes a binary tree as an argument
 * and checks if values stored in `BinNode`s are unique.
 */

:: BinTree a = BinNode a (BinTree a) (BinTree a) | TreeEnd

treeToList :: (BinTree a) -> [a]
treeToList TreeEnd = []
treeToList (BinNode x l r) = treeToList l ++ [x] ++ treeToList r

bt1 = (BinNode 3 (BinNode 2 TreeEnd (BinNode 4 TreeEnd TreeEnd)) (BinNode 1 (BinNode 5 TreeEnd TreeEnd) TreeEnd))
bt2 = (BinNode 0 TreeEnd TreeEnd)
bt3 = (BinNode 10 (BinNode 11 bt1 bt2) (BinNode 12 TreeEnd bt2))

isUnique :: (BinTree Int) -> Bool
isUnique TreeEnd = True
isUnique t1 = (treeToList t1) == removeDup (treeToList t1) 

//Start = isUnique TreeEnd // True
//Start = isUnique bt1 // True
//Start = isUnique bt2 // True
Start = isUnique bt3 // False