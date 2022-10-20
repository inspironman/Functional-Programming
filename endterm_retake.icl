module endterm_retake 

import StdEnv 

// Please fill the data required below.
//<Name>
//<Neptun_code>
//Functional Programming & end-term retake
//2022.Jan.10 
//This solution was submitted and prepared by <Name, Neptun_code> for the end-term retake
//assignment of the Functional Programming course.
//I declare that this solution is my own work.
//I have not copied or used third party solutions.
//I have not passed my solution to my classmates, neither  made it public.
//Students’ regulation of Eötvös Loránd University (ELTE Regulations Vol. II. 74/C.) 
//states that as long as a student presents another student’s work - 
//or at least the significant part of it - as his/her own performance, it will count as a disciplinary fault. 
//The most serious consequence of a disciplinary fault can be dismissal of the student from the University.


/* 1 - Removal
    Given an array of integers, remove the elements that have even occurrences in the array. 
*/
Occ :: Int {Int} -> Int
Occ num arr = sum [ 1 \\ x <-:arr | num == x ]

//Start = Occ 6 {1,1,2,2,2,3,3,4,5,6,6,6,6}


removeOcc :: {Int} -> {Int}
removeOcc arr = { x \\ x <-: arr | isOdd (Occ x arr) }

//Start = removeOcc {1,1,2,2,2,3,3,4,5,6,6,6,6} //{2,2,2,4,5}
//Start = removeOcc {1,1} // {}
//Start = removeOcc {1,1,1} // {1,1,1}
//Start = removeOcc {1,1,2,2,3,4,5} // {3,5,6}


:: City = BUDAPEST | GYOR | DEBRECEN  
:: Product = {productName :: String , price :: Real} 
:: Shop = {shopName :: String , products :: {Product}, location :: City}

meat       ={productName ="meat"   ,price= 5000.123 }
fruits     ={productName ="fruits" , price=2000.123 }
vegetables ={productName ="vegetables",price=1700.50}

aldi = {shopName = "aldi" , products = {meat,fruits,vegetables} ,location = BUDAPEST}
spar = {shopName = "spar" , products = {{productName ="meat"   ,price= 4500.0},fruits,vegetables} , location = GYOR}
lidl = {shopName = "lidl" , products = {meat,fruits,vegetables} ,location = BUDAPEST}
abc  = {shopName = "abc" , products =  {{productName ="meat"   ,price=  4500.0},{productName ="fruits"   ,price=  1700.0},{productName ="vegetables"   ,price=  1500.0}} , location = GYOR}


/* 2 - Shops 
    Given an array of shops, all the shops have the same products but with different prices,
    return a tuple containing the name of the cheapest shop and its location. The cheapest shop is
    the shop in the array whose sum of its products prices is the smallest in the array.
    
    Note : if there is more than one cheap shop in the array, return the first one.
    Assume that the given array is not empty. 
*/

cheap :: Shop -> Real
cheap s =  sum [x.price \\ x <-: s.products]

//Start = cheap aldi

cheapComp :: {Shop} -> [Real]
cheapComp list = [ cheap s \\ s <-: list ]

//Start = (cheapComp {aldi,spar,lidl,abc})

Aux1 :: {Shop} -> String
Aux1 list = hd [ x.shopName \\ x <-: list | minList (cheapComp list) == cheap x ]

Aux2 :: {Shop} -> City
Aux2 list = hd [ x.location \\ x <-: list | minList (cheapComp list) == cheap x ]

 
//Start = Aux1 {aldi,spar,lidl,abc}

cheapestShop :: {Shop} -> (String,City)
cheapestShop list = (Aux1 list,Aux2 list)

//Start = cheapestShop  {aldi,spar,lidl,abc} // ("abc",GYOR)
//Start = cheapestShop  {aldi,spar} // ("spar",GYOR)
//Start = cheapestShop  {lidl,aldi} // ("lidl",BUDAPEST)


/* 3 -	Diagonal offset
    Given an NxN matrix. Give the multiplication of its diagonal elements with regard to a given offset. For example:	
	{   {1,2,3},
		{4,5,6},
		{7,8,9}  }
		And let's assume that the given offset is 1. Therefore, the diagonal sum that we need is:
	
	{	{ 1, *2 , 3},
		{ 4,  5, *6},
		{ *7, 8,  9}  }
	The chosen elements are the ones with '*' before them. We start from the first element and offset its index by the given offset. Then
	we head to the next row and choose the element in diagonal with regard to the offset.
	Example2:
	
	{	{1,2,3},
		{4,5,6},
		{7,8,9}  } 
	And the given offset is 0. Therefore, the answer will be:
	{	{*1, 2, 3},
		{ 4,*5, 6},
		{ 7, 8,*9}  }
	Which is the normal main diagonal of the matrix.
*/
getLen :: {{Int}} -> Int
getLen list = length [y \\ y <-: list]

//Start = size {{1,2,3},{4,5,6},{7,8,9}}

//getDiagMul :: {{Int}} Int -> [Int]
getDiagMul list num =  [ x.[y] \\ x <-: list & y <- [num..((getLen list)-1)]++[num-1..(getLen list)-1]]

//Start = getDiagMul {{1,2,3},{4,5,6},{7,8,9}} 0	// 45
//Start = getDiagMul {{1,2,3},{4,5,6},{7,8,9}}  1   // 84
//Start = getDiagMul {{1,2},{4,5}}  1 				// 8
/*Start = getDiagMul {{1,2,3,4,7},
					{4,5,6,7,8},
					{7,8,9,7,3},
					{1,2,3,4,2},
					{4,2,6,1,8}}  2
*/

::Person   = {name :: String, age :: Int, numbers :: {Int}}
abdullah :: Person 
abdullah   = {name = "Abdullah", age = 13 , numbers = {-4,3,2,1}  }
abood    :: Person 
abood      = {name = "abood", age = 12 , numbers =  {3,-6,5,-2,1} }
othman   :: Person
othman     = {name = "othman", age = 12 , numbers =  {-5,4,-2,3,1} }
mohammed :: Person
mohammed   = {name = "Mohammed" , age = 18 , numbers =  {-5,4,-2,3,1,-1,-6,-1,0,5}}

/* 4 -  StayPositive
    Write the StayPositive function defined bellow which gets an array of Persons and do the following: 
        for each Person in the given array, there is an array of numbers, start with some value x 
        and add each of the array elements to it consecutively. That is, calculate running sum of x plus
        each of the array elements, return the minimum value of x such that the running sum is always 
        at least 1, the value of x can never be less than one!

    Example :
        Abdullah = {name = "Abdullah", age = 12 , numbers = {-4,3,2,1}}
        The numbers array is :  {-4,3,2,1}
        if x = 5, the running sums are:
        5 + (-4) = 1 
        1 + 3    = 4
        4 + 2    = 6
        6 + 1    = 7. 
    There is no smaller value for x that satisfies the condition.
    The function should return list of these minimum values.
*/ 

//running :: {Int} -> Int
//running arr = [ x+y \\ x <-: arr & y <- [0..] | 

//StayPositive :: {Person} ->  [Int]

//Start = StayPositive {abdullah,abood,othman} // [5,4,6]
//Start = StayPositive {mohammed} // [8]
//Start = StayPositive {} // []



/* 5 and 6 - Vector
   BE AWARE THAT THIS TASK CONSISTS OF MULTIPLE SMALL TASKS. THUS, IT IS WORTH DOUBLE POINTS (20 POINTS). 
*/
                                       
:: Vector a :== [a]

/*
    A vector in programming is a dynamic implementation of the Array data structure
    We will define this type and create the pushBack, pushFront, remove, indexOf, and swap operations for this type.

    a)
        pushBack is a function that takes a vector and an element and adds the element to the end of the vector,
        pushFront is a function that takes a vector and an element and adds the element to the beginning of the vector.
        Complete the pushBack and pushFront functions.
*/
pushBack  :: (Vector a) a -> (Vector a)
pushBack vec a = vec ++ [a]

pushFront :: (Vector a) a -> (Vector a)
pushFront vec a = [a] ++ vec

//Start = pushBack [1,2,3] 4 //[1,2,3,4]
//Start = pushBack [1,0,213] 10000 //[1,0,213,10000]
//Start = pushFront [1,0,213] 10000 //[10000, 1,0,213]
//Start = pushBack [1,2,3] 4 //[4,1,2,3]


/*
   b) 
      remove is a function that takes a vector and an element and removes the element from the vector,
      if it exists, and returns the modified vector. Otherwise, it returns an error.
      Complete the remove function.
*/



remove :: (Vector Int) Int -> (Vector Int)
remove vec i 
| length [ x \\ x <- vec | x <> i ] > 0 = [ x \\ x <- vec | x <> i ]
= abort "Element does not exist"
 
//Start = remove [1,2,3] 2 //[1,3] 
//Start = remove [1,0,213] 10000 //"Element does not exist"


/*
  c)
     indexOf is a function that takes a vector and an element and returns the element's index in the vector,
     if it exists, otherwise it returns an error.
     Complete the indexOf function.
*/
indexOf :: (Vector Int) Int -> Int
indexOf vec a  = x!!0 


where 
     x = [ i \\ x <- vec & i <- [0..] | x == a ]

//Start = indexOf [1,2,3] 2 // 1 
//Start = indexOf [1,0,213] 10000 //"Element does not exist"

/*
  d)
     swap is a function that takes a vector and two elements and swaps the two elements in the vector,
     if they both exist, otherwise it returns an error.
     Complete the swap function.
*/
swap :: (Vector Int) Int Int -> (Vector Int)
swap vec a b = (insertAt Y b (removeAt Y (insertAt X a (removeAt X vec))))
where 
     X = indexOf vec a
     Y = indexOf vec b

//Start =  removeAt 2 [1,2,3,5,6]

//Start = swap [1,2,4,5,6,3,888,9,7] 1 3 // [3,2,4,5,6,1,888,9,7] 
//Start = swap [1,0,213] 10000 0 //"Element does not exist"


:: FlexTree a = TernaryNode a (FlexTree a) (FlexTree a) (FlexTree a)
              | BinaryNode a (FlexTree a) (FlexTree a)
              | UnaryNode a (FlexTree a)
              | TerminalNode 

ftree1 = UnaryNode 1 (BinaryNode 2 TerminalNode TerminalNode)
ftree2 = BinaryNode 1 TerminalNode ftree1
ftree3 = TernaryNode 3 TerminalNode (UnaryNode 3 TerminalNode) (UnaryNode 3 TerminalNode)
ftree4 = TernaryNode 1 ftree2 TerminalNode (BinaryNode 2 (TernaryNode 1 TerminalNode TerminalNode TerminalNode) (BinaryNode 2 ftree2 ftree3))


/* 7 - Flex Tree
	FlexTree can have 4 types of nodes: Ternary, Binary, Unary and Terminal. As
	names suggest these nodes have 3, 2, 1 and 0 children nodes respectively. 
	Terminal nodes do not store any value; they indicate end of the tree.

    Write a function `flexTreeSum` that gets an integer flextree as an argument
    and returns the sum of every value from this tree. You can assume that sum of 
    TerminalNode is 0.
*/
flexTreeSum :: (FlexTree Int) -> Int
flexTreeSum (TerminalNode ) = 0
flexTreeSum (UnaryNode x l) = x + flexTreeSum l
flexTreeSum (BinaryNode x l r) = x + flexTreeSum l + flexTreeSum r
flexTreeSum (TernaryNode x l m r) = x + flexTreeSum l + flexTreeSum m + flexTreeSum r


//Start = flexTreeSum TerminalNode // 0
//Start = flexTreeSum ftree1 // 3
//Start = flexTreeSum ftree2 // 4
//Start = flexTreeSum ftree3 // 9
//Start = flexTreeSum ftree4 // 23


/* 8 - Words
      Let Word be type synonym of String. 
      Given a list of words, create an instance < for words to sort it using the sort function.
      A word is greater than the other one if its sum of characters' ASCI values are greater.
      To calculate sum: if the character is uppercase, it should be multiplied by 2.
                        if the character is lowercase, it should be subtracted by 22.    
      Hint: The lowercase letters are in the [97,122] range,
            the uppercase letters are in the [65,90] range in the ASCI table.
*/
:: Word :== String

//instance < Word
//where
//    (<) a = sum [toInt x \\ x <-: a]
 
//instance < Word | Ord 
//where 
//     (<) a b = sum [toInt x \\ x <-: a] < sum [toInt y \\ y <-: b]
     
//Start = sort ["abcde", "ABCD", "functional", "Functional", "PROGRAMMING", "ELTE", "year"] // ["year","abcde","ABCD","ELTE","functional","Functional","PROGRAMMING"]

:: RoseTree a =  TNode a [(RoseTree a)]
tree1 = TNode 5 []
tree2 = TNode 3 [(TNode 2 [(TNode 3 [])]), tree1, tree1]
tree3 = TNode 1 [tree1, tree2, tree2]





/* 10 - Merge
    Create a class Merge which has operations sorted, mess and has the neutral element Empty. 
    The sorted and mess are performing the following operations, and create an instance for [Int].
    sorted -> merges sorted lists and returns sorted list. If a list is not sorted, replace it with 
                  empty list and merge.
    
    mess -> merges lists from the beginning of the first one and follows from the last of the second. 
            
    for instance: mess [1,2,3,5] [9,8,10] = [1,10,2,8,3,9,5]
            
    Empty -> empty list
*/
// class Merge..
    
// instance Merge..

//Start = mess [1,2,3,5] [9,8,10] // [1,10,2,8,3,9,5]
//Start = sorted [1..10] [7..15] // [1,2,3,4,5,6,7,7,8,8,9,9,10,10,11,12,13,14,15]
//Start = sorted [3..7] Empty // [3,4,5,6,7]
//Start = sorted Empty [1,3,7,4,2] // []
//Start = mess Empty [1..10] // [10,9,8,7,6,5,4,3,2,1]


/* 11 - Names
        Name is type synonym of String.
	    Write an instance == for names which returns True if names' first and last letters are
	    same or if either 'o' or 'O' have been contained in both names.
	    Upper and lower characters will be considered different.
	    For instance: "bOris" == "Boris" -> True /even first characters are different, both names
											  has 'O' and 'o' respectively./ 

*/
::Name :== String

instance == Name
where
     (==) a b = (hd [ x \\ x <-: a ] == hd [ x \\ x <-: b ]) || (tl [ x \\ x <-: a ] == tl [ x \\ x <-: b ]) || isMember 'o' [ x \\ x <-: a ] || isMember 'O' [ x \\ x <-: a ] || || isMember 'o' [ x \\ x <-: b ] || isMember 'O' [ x \\ x <-: b ]

Start = ["sah"=="sarah", "bOris"=="Boris", "David"=="david", "john"=="Jean", "Edmund"=="Erand"] //[True,True,False,False,True]


/* 12 - IsBTree
    Write a function that takes a Rosetree as an argument
    and checks if it can be converted into binary tree.
    In binary tree each node should have at most 2 children.
*/
//isBTree :: (RoseTree a) -> Bool

//Start = isBTree tree1 // True
//Start = isBTree tree2 // False
//Start = isBTree tree3 // False
//Start = isBTree (TNode 1 [(TNode 2 []), (TNode 3 [])]) // True
//Start = isBTree (TNode 1 [(TNode 2 [])]) // True









