module midsem3 
import StdEnv

/*1
  * Write a function, that takes a list of functions, and a list of
  * tuples (Int, Int) where the first Int indicates which function to
  * use and the second Int acts as a parameter and returns a list of
  * the results.
  
  * For example: Router [isEven,isOdd] [(1,2),(2,4),(1,57)] = [True, False, False]
  */
  
RouterAux :: [ (a->b) ] (Int,a) -> b
RouterAux listFun (a,b) = (listFun !! (a-1)) b

//Start = RouterAux [ isEven,isOdd  ] (2,6)

Router :: [ (a -> b) ] [(Int,a)] -> [b]
Router [] _ = []
Router _ [] = []
Router listFun list = map (RouterAux listFun) list

//Start = Router [isEven,isOdd] [(1,2),(2,4),(1,57)] //[True, False, False]

//Start = Router [((+)1),((*)2),((^)2),((rem) 100)] [(4,13),(2,23),(3,5),(1,1336),(4,23)] //[9,46,32,1337,8]

//Start = Router [(\x = [1..x]),(\x = [n\\n<-[1..x]|x rem n ==0]),(\x = [x,x*2..x*10])] [(2,36),(1,13),(3,5),(2,128),(3,1)]  //[[1,2,3,4,6,9,12,18,36],[1,2,3,4,5,6,7,8,9,10,11,12,13],[5,10,15,20,25,30,35,40,45,50],[1,2,4,8,16,32,64,128],[1,2,3,4,5,6,7,8,9,10]]

//Start = Router [] [(4,13),(2,23),(3,5),(1,1336),(4,23)] //[]

//Start = Router [isEven,isOdd] [] //[]


/**2
  * Write a function that takes a list of integers and returns a list of
  * result integers based on how many integers were in the parameter list.
  * For 1 integer 'a', it will return that integer modulus 2. (a rem 2)
  * For 2 integers 'a','b' , it will return a list of all integers from the first to the second. [a..b]
  * For 3 integers 'a','b','c' , it will return (a*(b^c))
  * For 4 integers 'a','b','c','d', it will return a list of the sum of 'a' and 'b' and the sum of 'c' and 'd'.
  */

Listing :: [Int] -> [Int]
Listing list 
| length list == 1 = [ x rem 2 \\ x <- list ]
| length list == 2 = [ (hd list)..(last list)]
| length list == 3 = [ (hd list) * ( (list !! 1) ^ (last list) )  ]
| length list == 4 = [ (hd list + (list !! 1)), (list !! 2) + (last list) ]
= [] 
  
  
  
//Start = Listing [5] //[1]

//Start = Listing [4,10] //[4,5,6,7,8,9,10]

//Start = Listing [3,5,2] //[75]

//Start = Listing [13,29,1030,307] //[42,1337]

//Start = Listing [] //[]

/**3
  * Write a function that checks if a list of numbers is odd,even,odd,even...
  
  * For exmaple: SeqCheck [1,2,3,4,6] = False because 4 is even, but 6 is not odd.
  */
SeqCheck :: [Int] -> Bool
SeqCheck [] = False
SeqCheck list = and [ (isEven x && isEven y) || (isOdd x && isOdd y) \\ x <- list & y <- [1..] ]

//Start = SeqCheck [1..10] //True

//Start = SeqCheck [1,2,3] //True

//Start = SeqCheck [2,3,4] //False

//Start = SeqCheck [1,3,4,5] //False

//Start = SeqCheck [1,2,3,4,6,7] //False

//Start = SeqCheck [] //False

/**4
  * Write a function that checks if each elements in the list appear even times.
  
  * For example, checkEven [1,1,2,2,2,2,3,5,3,5] = True
  */

checkEvenAux :: [Int] Int -> Int
checkEvenAux [] num = 0
checkEvenAux list num = length ( filter ((==)num) list )

//Start = checkEvenAux [1,1,2,2,2,2,3,5,3,5] 2

checkEven :: [Int] -> Bool
checkEven [] = False
checkEven list = and [ isEven (checkEvenAux list x ) \\ x <- list ]

//Start = checkEven [1,1,2,2,2,2,3,5,3,5] 
//Start = checkEven [1,1,2,2,1] // False
//Start = checkEven [] //False

/**5
  * Write a function that takes two vectors, represented as lists, and returns their dot product.
  
  * The dot product of two vectors can be computed as:
  
  * < xa, xb, xc, ...> * < ya, yb, yc, ...> = (xa*ya) + (xb*yb) + (xc*yc) + ...
  
  * For example: DotProd [4,6,3] [6,3,7] = 24+18+21 = 63
  */

DotProd :: [Int] [Int] -> Int
DotProd list1 list2 = sum [ x*y \\ x <- list1 & y <- list2 ]

//Start = DotProd [4,6,3] [6,3,7]

/*
*6
// Given a list of characters, split it into a tuple in which the first part only contains digits 
  ('0'..'9'),
// the second part contains the rest.
*/
TwoListAux1 :: [Char] -> [Char]
TwoListAux1 list = [ x \\ x <- list | x >= '1' && x <= '9' ]

TwoListAux2 :: [Char] -> [Char]
TwoListAux2 list = [ x \\ x <- list | x >= 'a' && x <= 'z' ]


//Start = TwoListAux1  ['1', 'a', '2', 'b', '3'] 

TwoList :: [Char] -> ([Char],[Char])
TwoList list = ( TwoListAux1 list , TwoListAux2 list ) 

//Start = TwoList  ['1', 'a', '2', 'b', '3'] // (['1','2','3'],['a','b'])

/*
*7 
Given a list of lists, for each list, extract the first, middle and last element.
*/

Points3 :: [[Int]] -> [(Int,Int,Int)]
Points3 [[]] = []
Points3 list = [ (hd x , x !! ((length x)/2), last x) \\ x <- list ]


//Start = Points3 [[1..9], [2..6], [3..11], [1..10]] // [(1,5,9),(2,4,6),(3,7,11),(1,6,10)]

//Start = Points3 [[]] //[]

/*  
*8
//Find the 'unique' right triangle in the list eg. (3,4,5) and (4,3,5) are the same triangle. 
//only one will appear in the answer list [(3,4,5),(4,3,5)] -> [(3,4,5)] 
*/

f8Aux :: [(Int,Int,Int)] -> [[Int]]
f8Aux list = removeDup [ sort [x,y,z] \\ (x,y,z) <- list ]

f8 :: [(Int,Int,Int)] -> [(Int,Int,Int)]
f8 list = [ (hd x , x !! ((length x)/2), last x) \\ x <- f8Aux list |( ((hd x)^2) + (x !! (((length x)/2))^2) == ((last x)^2) ) && (hd x) > 0 && ((length x)/2) > 0 && (last x) > 0 ]


//Start = f8 [(3,4,5),(4,5,6),(4,5,3),(6,8,10),(10,5,8),(-3,4,5)] //[(3,4,5),(6,8,10)]

//Start = f8 [(1,1,1),(5,4,3),(3,4,5),(0,0,0)] //[(5,4,3)]

/*
*9
//Use foldr to check if the square root of each integer in a list are all integers. 
*/














/*
*10 Insert sum of elements as last element in every sublist of a list. 
*/

addSum :: [[Int]] -> [[Int]]
addSum list = [ x ++ [ sum x ] \\ x <- list ]

//Start = addSum [[1,2], [3,4,5], [6,5,9,7], [], [8]] 


































































  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
