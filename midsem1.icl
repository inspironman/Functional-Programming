module midsem1
import StdEnv


/* 
 1. Armstrong number

 If sum of cubes of each digit of the number is equal to the number itself,
 then the number is called an Armstrong number.
 153 = 1^3 + 5^3 + 3^3
 Given a positive integer number, write a function to determine whether it is
 an Armstrong number or not.

*/

digit :: Int -> [Int]
digit num 
| num > 9 = [ num rem 10 : digit (num/10) ] 
= [num]

//Start = digit 153 

Armnum :: Int -> Bool 
Armnum num =  num == sum [ x*x*x \\ x <- digit num ]

//Start = Armnum 153

/* 2. Occurrences

 Given a list of integers, replace every element in the list with its number
 of occurrences in the list.

*/
OccAux :: [Int] Int -> Int
OccAux list num = length (filter ((==) num) list)

//Start = OccAux [1,2,3,1,13,4,42,21,34,22,44,23,32,1] 1

Occ :: [Int] -> [Int]
Occ list = [ OccAux list x \\ x <- list ]

//Start = Occ [1,2,3,1,2,1]  

/* 
 
 3. Gap2
 
 Given a list of numbers, convert the list in such a way that 
 the difference between two consecutive elements is always 2,
 you may have to add numbers in order to achieve that.
 e.g: [1,5,8] = [1,3,5,7,9]

*/

Gap2 :: [Int] -> [Int]
Gap2 list = [minList list, (minList list)+2..(maxList list)+1]

//Start = Gap2 [1,9]

/* 4. Not Palindrome
 Given a list of lists of integers,
 write a function that gets rid of Palindrome numbers.
 A palindrome number is a number that can be read from left to right or
 from right to left and gets the same number, 
 e.g. 12521 is a palindrome number. 
*/


npalin :: Int -> Bool
npalin num 
| num < 10 = True
| num > 10 = digit num <> reverse (digit num) 
= False  

//Start = npalin 12321

getRidPal :: [[Int]] -> [[Int]]
getRidPal list = [filter npalin x \\ x <- list ]

//Start = getRidPal [[1,2,1111],[151,22,3455]] // [[1,2],[3455]]
//Start = getRidPal [[1,222],[151,202,50505]] // [[1],[]]
//Start = getRidPal [[],[22]] // [[],[]]


/* 5. Not Primes
 Given a list of integers, write a function which removes the prime numbers from the list.
 There will be no negative integers and consider the number 1 not a prime.
*/

isPrime :: Int -> Bool
isPrime num = length ([ x \\ x <- [1..num] | num rem x == 0 ]) == 2

//Start = notPrime 6

noPrime :: [Int] -> [Int]
noPrime list = [ x \\ x <- list | isPrime x == False ]

//Start = noPrime [1..10] // [1,4,6,8,9,10]
//Start = noPrime [13..20] // [14,15,16,18,20]
//Start = noPrime [2,4,8,9,10,23] // [4,8,9,10]
//Start = noPrime [] // []

/* 6. zipWith

 Implement the function zipWith that takes a function, 
 and two lists, and combines them in such a way that 
 elements that are in the same positions get the function 
 applied to them.

 E.g: zipWith addTwoNumbers [1,2,3] [5,6,7] = [1+5,2+6,3+7] = [6,8,10]
*/
//DON'T DELETE THESE FUNCTIONS !!!
addTwoNumber x y = x + y
prodTwoNumber x y = x * y
niceTwoNumber x y = x rem y
//

zipWith :: (Int Int -> Int) [Int] [Int] -> [Int]
zipWith func l1 l2 = [ func x  y \\ x <- l1 & y <- l2 ]

//Start = zipWith addTwoNumber [1,2,3] [5,6,7] // [6,8,10]
//Start = zipWith prodTwoNumber [1,2,3] [5,6,7] // [5,12,21]
//Start = zipWith niceTwoNumber [5,6,7] [1,2,3] // [0,0,1]

/* 7. Collatz conjecture

 Given a positive number greater than 1, return how many iterations does it 
 take for that number to fall down to "2" if we keep applying the
 Collatz equation on it.
 Collatz conjecture equation:
 If the number is even -> x/2
 If the number is odd -> 3x+1
 e.g: input: 10 
      steps: 10 -> 5 -> 16 -> 4 -> 2
      output: 4 recursive calls
*/

colla :: Int -> Int
colla 2 = -1
colla num
| num > 2 && num rem 2 == 0 = colla (num/2) + 1
| num > 2 && num rem 2 == 1 = colla (3*num + 1) + 1
= abort "The number must be greater than 1"

//Start = colla 6
//Start = colla 10 // 4
//Start = colla 20000000 // 144
//Start = colla 5 // 3
//Start = colla 0 // "The number must be greater than 1"


collatz :: Int -> Int
collatz num
| num < 2 = abort "Number must be greater than 1\n"
= (length (takeWhile ((<=)2) [y \\ y <- (iterate (\x | isEven x = (x/2) =((3*x) + 1)) num)]))-2

//Start = collatz 10 // 4
//Start = collatz 20000000 // 144
//Start = collatz 5 // 3
//Start = collatz 0 // "The number must be greater than 1"

/* 8. Good Lists

 Given a list of lists, count how many of the sublists are good lists.
 A list is good if it is empty or its 1st number is odd, 2nd is even, 
 3rd is odd, 4th is even and so on.
 E.g: [[],[1,2,3,4],[8,3,4],[9],[3,4,4]] your function should return
 3 as only [], [1,2,3,4] and [9] are "good".
*/

check :: [Int] -> Bool
check list = and [ (isEven x && isEven y) || (isOdd x && isOdd y) \\ x <- list & y <- [1..] ]  

//Start = check [1,2,3,4,5]

goodList :: [[Int]] -> Int
goodList [] = 0
goodList [x:xs]
| check x = 1 + goodList xs
= goodList xs

//Start = goodList [[],[1,2,3,4],[8,3,4],[9],[3,4,4]]
//Start = goodList [[1,2,3,4],[3,4,4],[3,42],[12,2,1,2]]


/* 9. Symmetrical lists
 Given a list of lists of integers, write a function 
 which returns a list of symmetrical lists, 
 if the sum of the sublist is greater than 10.
*/

sym :: [[Int]] -> [[Int]]
sym list = [ x ++ reverse x \\ x <- list | sum x > 10 ]


//Start = sym [[1,2,3],[3,4,5,6],[4,5,1,2]] // [[3,4,5,6,6,5,4,3],[4,5,1,2,2,1,5,4]]
//Start = sym [] // []
//Start = sym [[1..10],[1,2]] // [[1,2,3,4,5,6,7,8,9,10,10,9,8,7,6,5,4,3,2,1]]

/* 10. Elements in interval

 Given a list of triple tuples consisting of two integer values and 
 and a list of integers (left,right,[Int]),
 for every tuple return only the elements from the list 
 which positions' are inside the interval [left..right]
 Assume that the indexes are all valid.
*/

getList :: Int Int [Int] -> [Int]
getList x y list = [ a \\ a <- list & b <- [0..] | isMember b [x..y] ]

elementInInterval :: [(Int,Int,[Int])] -> [[Int]]
elementInInterval list = [ getList a b list \\ (a,b,list) <- list ]

//Start = elementInInterval [(2,5,[1..10])] //[[3,4,5,6]]
//Start = elementInInterval [(5,6,[1..8]), (3,5,[4..9])] //[[6,7],[7,8,9]]
//Start = elementInInterval [(4,7,[1,2,3,4,5,6,7,8,9])] //[[5,6,7,8]]

eleAux :: Int Int [Int] -> [Int]
eleAux x y list = [ list !! b  \\  b <- [x..y] ]

//Start = eleAux 4 6 [1,2,3,45,5,5,53,32,21]

eleInt :: [(Int,Int,[Int])] -> [[Int]]
eleInt list = [ eleAux (fst3 x) (snd3 x) (thd3 x) \\ x <- list ]

Start = eleInt [(5,6,[1..8]), (3,5,[4..9])] //[[6,7],[7,8,9]]













