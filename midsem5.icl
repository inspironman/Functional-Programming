module midsem5
import StdEnv

/*1. List ends
 Given a list of lists, append to the end of every sublist 
 the sum and the length of the sublist
*/

append :: [[Int]] -> [[Int]]
append biglist = [ list  ++ [ sum list ] ++ [length list]\\ list <- biglist ]

//Start = append [[1..5],[1..4],[],[5,6]]  // [[1,2,3,4,5,15,5],[1,2,3,4,10,4],[0,0],[5,6,11,2]]
//Start = append [[(-1),(-2)..(-10)],[12],[5]]  // [[-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-55,10],[12,12,1],[5,5,1]]
//Start = append []  // []

/* 2. Fractions
 
 Given a list of real numbers, keep only the fraction part of the numbers.
*/

fraction :: [Real] -> [Real]
fraction list = [ x - toReal (abs x) \\ x <- list ]



//Start = fraction [1.2,1.5,0.6,1.0] //[0.2,0.5,0.6,0]
//Start = fraction [1.25, 8.2115548896, 53.21,45.58,0.005] //[0.25,0.2115548896,0.21,0.58,0.00005]
//Start = fraction [] // []

/*3. Famous nums

 Given a list of integers, write a function which gets rid of the numbers that is occurring
 less than 5 times in the list.
*/



famousNumAux :: [Int] -> [Int]
famousNumAux list = filter ((==) x) y 
where 
   x = hd (sort list)
   y = sort list
   
//famousNum :: [Int] -> [Int]
//famousNum list 
//| length famousNumAux >= 5 = take (length famousNumAux) list ++ drop  

//Start = famousNum [1,1,1,1,1,1,2,3,4,4,4,4,5,5,5,5,5] // [1,1,1,1,1,1,5,5,5,5,5]
//Start = famousNum [] // []
//Start = famousNum [1,2,3,4,5,6,1,1,1,2,2,2,2,1,1,5,10,3] // [1,2,1,1,1,2,2,2,2,1,1]


/*4. Search
 
 Implement a search algorithm that searches through a list for Int n and returns the value in the list before n. 
 If there is no value, or the list is empty, return -1. e.g., findPrev 5 [1,2,3,4,5,6] should return 4, 
 while findPrev 5 [0, 10, 20, 30] returns -1. If there are multiple of such element, consider the first.
*/


findPrev :: [Int] Int -> Int
findPrev [] num = hd [-1]
findPrev list num 
| hd list <> num = hd [list !! (x-1) \\ x <- [0..length list] | list !! x == num]
= -1

//Start = findPrev [1,2,3,4,5,6] 5 // 4
//Start = findPrev  [1,2,3,4,5,6]  1 // -1
//Start = findPrev [] -1 // -1 

/* 5. Symmetric difference 

 Given two lists of integer numbers , return a sorted list containing the symmetric difference of the two lists; 
 The symmetric difference of two lists A and B is the list (A � B) U (B � A); 
 where A - B is The difference of two lists  defined as follows:  
 The List A-B consists of elements that are in A but not in B.
 And (U) the union of two lists is a list containing all the elements of A and B without duplicates 
*/

symetricDif :: [Int] [Int] -> [Int]
symetricDif list1 list2 = [ x \\ x <- list1 | isMember x list2 == False  ] ++ [ x \\ x <- list2 | isMember x list1 == False  ]

//Start = symetricDif  [1,2,3,4,5] [2,4,6] //  [1,3,5,6]
//Start = symetricDif  [1..5] [1..10] // [6,7,8,9,10]
//Start = symetricDif  [1..5] [] // [1,2,3,4,5]



/*6. Not N

 Given a list of integers and an integer N, 
 eliminate from the list elements that are positioned before N in the list and are not equal to N,
 then compute the biquadrate of the numbers left in the list.
*/

notN :: Int [Int] -> [Int]
notN num list = [ x^4 \\ x <- takeWhile ((<>) num) list ]



//Start = notN 3 [1..5] // [1,16]
//Start = notN 10 [] // []
//Start = notN 6 [10,8..1] // [10000,4096]

/* 7.  Gap2 continued 

 Given a list of numbers, return True if the  
 the difference between two consecutive elements is always 2
 otherwise return False
*/

gap2C :: [Int] -> Bool
gap2C [] = False 
gap2C list = and [ (list!!(x+1) - list!!x) == 2 \\ x <- [0..((length list)-2)] ]

//Start = gap2C [1,3,5,7] // True
//Start = gap2C [1,3,5,7,9,11,13,15,17] // True
//Start = gap2C [1,5,8] // False
//Start = gap2C [] // False


/* 8. Good Lists
 Given the list of lists and a list of unique numbers. 
 Numbers that are given in this second unique number list are considered to be good numbers. 
 A List is considered good if at least half of its numbers are good. Count how many good lists 
 are in the given list of lists.

 Ex. If you are given [[1,2,3], [1,3,3,4,9,6], [3..6]]  and [1,2,3], good numbers are 1, 2 and 3. 
 First list [1,2,3] has 3 good numbers out of total 3 numbers, hence it is good. 
 Next one [1,3,3,4,9,6] has 3 good numbers (1,3,3) which is half of total length, hence it is a good one as well.
 Last list [3..6] has only one good number and is not a good list. Therefore, answer for this example is 2.
*/

goodListsAux :: [Int] [Int] -> Bool
goodListsAux [] x = False
goodListsAux x [] = False
goodListsAux list1 list2 
| length list1 > length list2 = length [ x \\ x <- list1 |  isMember x list2 ] >= ((length list2)+1)/2
= length [ x \\ x <- list2 |  isMember x list1 ] >= ((length list1)+1)/2

//Start = goodListsAux [3..6] [1,2,3]
//Start = goodListsAux [1] [1,2,3,8]

goodLists :: [[Int]] [Int] -> Int
goodLists list1 list2 = length [ x \\ x <- list1 | goodListsAux x list2 ]


//Start = goodLists [[1,2,3], [1..6], [3..6]] [1,2,3] // 2
//Start = goodLists [[1], [1..6], [3,8,5]] [1,2,3,8] // 3
//Start = goodLists [[], [3,2,5], [1,1,2,2]] [1] // 1
//Start = goodLists [] [1,2,3] // 0

/*9. CoPrimes
 Given 2 numbers, check if they are co-prime.
 Numbers are called co-prime if they do not have
 common divisor.
*/

coAux1 :: Int -> [Int]
coAux1 num1 = [ x \\ x <- [2..num1] | num1 rem x == 0]

coAux2 :: Int -> [Int]
coAux2 num2 = [ y \\ y <- [2..num2] | num2 rem y == 0 ]


coPrimes :: Int Int -> Bool
coPrimes num1 num2 =  length [ x \\ x <- coAux1 num1 | isMember x (coAux2 num2) ] == 0


//Start = coPrimes 12 9 // False
//Start = coPrimes 12 12 // False
//Start = coPrimes 12 13 // True
//Start = coPrimes 5 7 // True

/* 10. Clean Sequence
 The Clean sequence is defined in following way:
 s(0) = a
 s(1) = b
 s(2) = c
 and for every k greater than 2:
 s(k) = ( s(k-1)*s(k-2) + s(k-3) ) rem 1000
 
 Given n, a, b and c - generate first n numbers from Clean sequence.
*/

cAux :: Int Int Int Int -> Int
cAux 0 a b c = a
cAux 1 a b c = b
cAux 2 a b c = c
cAux k a b c = ( (cAux (k-1) a b c)*(cAux (k-2) a b c) + (cAux (k-3) a b c) ) rem 1000


//Start = cAux 5 1 2 3

clean :: Int Int Int Int -> [Int]
clean n a b c = take n [cAux i a b c \\ i <- [0..]]
 

//Start = clean 5 1 2 3 // [1,2,3,7,23]
//Start = clean 11 123 79 3 // [123,79,3,360,159,243,997,430,953,787,441]
//Start = clean 2 1 2 3 // [1,2]
//Start = clean 1 1 2 3 // [1]

 




































































