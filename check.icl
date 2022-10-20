module check
import StdEnv
 


myBill :: Real Real -> Real 
myBill x y = x + (y*x)

//Start = myBill 10.00 0.15

/*
Given a list of Real numbers and an integer, keep the elements of the list,
that are divisble by the given integer and double them.
e.g: [3.0,1.0,0.0] 3 = [3.0] (since the integer part of 3.0 is divisible by the given integer 3)
*/

divAux :: Real -> Bool
divAux x = toInt x rem 3 == 0

//Start = divAux 9.0






/*
divList :: [Real] Int -> [Real]
divList 
*/
//Start = divList [3.0,1.0,0.0] 3 // [6.0]
//Start = divList [1.0,1.0,2.0,3.0,4.0,5.0,6.0,7.2,8.0] 3 // [6.0,12.0]
//Start = divList [1.0,0.0] 2 // []


/*
The function decimal takes a real number and returns the decimal part of the number,
write the implementation of the function.
*/

floor :: Real -> Real
floor x 
| toReal(toInt x) > x = toReal(toInt x) - 1.0
= toReal(toInt x)

//Start = floor 4.9  

decimal :: Real -> Real
decimal x = x - floor x

//Start = decimal 1.5 // 0.5
//Start = decimal 35.35 // 0.35
//Start = decimal 4.6 // 0.6
 
/* Write a function that takes a list

 * of numbers as an argument and finds

 * the difference between sum of numbers

 * on odd indexes and sum of numbers on even indexes.

 * i.e: (1st + 3rd + 5th + 7th + ...) - (2nd + 4th + 6th + ...)

 */



halfDiffSum :: [Int] -> Int
halfDiffSum [] = 0
halfDiffSum [x] = x
halfDiffSum [x,y:xs] = x - y + halfDiffSum xs

//Start = halfDiffSum [2, 3] // -1

//Start = halfDiffSum [3, 2] // 1

//Start = halfDiffSum [1] // 1

//Start = halfDiffSum [1,2,3,4,2,5] // -5

//Start = halfDiffSum [] // 0

//Start = halfDiffSum [2,2,2] // 2



//Given a list of element, find the element of the given index
find :: [Int] Int -> Int
find [] x = 0
find [x:xs] index
| index > (length [x:xs]) = abort "no element at this index"
| index == 0 = x
= find xs (index-1)
 
//Start = find [1..10] 4

//Given a list of element, remove the element of the given index
removeEle :: Int [Int] -> [Int]
removeEle x [] = abort "No element exist"
removeEle index [x:xs] 
| index > (length [x:xs]) = abort "no element at this index"
| index == 0 = xs
= [x] ++ removeEle (index-1) xs

//Start = removeEle 4 [1..10] 

// Create your own implementation of isMember function

myIsMem :: Int [Int] -> Bool
myIsMem x [] = False
myIsMem n [x:xs]
| x == n = True
|otherwise = myIsMem n xs

//Start = myIsMem 12 [5,10..500]
 
// Create your own implementation of insertAt function


myinsertAt :: [Int] Int Int -> [Int]
myinsertAt [] x y = [x]
myinsertAt [x:xs] num index
| index > length [x:xs] + 1 = abort "Index out of Range"
| index == 0 = [num] ++ [x:xs]
= [x] ++ myinsertAt xs num (index - 1)

//Start = myinsertAt [1..5] 5 2
 
// Given two sorted list, return list which contains their intersection

intersection :: [Int] [Int] -> [Int]
intersection [] _ = []
intersection [x:xs] list 
| isMember x list = [x] ++ intersection xs list
= intersection xs list
 
//Start = intersection [1,2,3,4,5] [1,2,3] 
 
/*
Given a list of integers, write a function which will keep only the number divisible by 10
but not divisible by 4, and remove the rest.
[10,20..100] = [10,30,50,70,90] 
*/

Divisible4_10 :: [Int] -> [Int]
Divisible4_10 [] = []
Divisible4_10 [x:xs]
| x rem 4 <> 0 && x rem 10 == 0 = [x] ++ Divisible4_10 xs
= Divisible4_10 xs

//Start = Divisible4_10 [10,20..100]

/*
Given a list of lists of Integers and an Integer.
Write a function which will return a list of the maximal elements of each list which are 
smaller than the given Int. It is guaranteed that each list has atleast one element less
than the given Int.
NOTE :- [[1,3],[1,4]] 2 = [1,1] and not []
*/

MaxTwo :: Int Int -> Int
MaxTwo x y
| x > y = x
= y

MyMax :: [Int] -> Int
MyMax [] = 0
MyMax [x:xs] = MaxTwo x (MyMax xs)
 
//Start = MyMax [1..10]

Maux :: [Int] Int -> [Int]
Maux [] n = []
Maux [x:xs] n
| x < n = [x] ++ Maux xs n
= Maux xs n

//Start = Maux [1..10] 9

MaxLessN :: [[Int]] Int -> [Int]
MaxLessN [] n = []
MaxLessN [x:xs] n = [MyMax (Maux x n)] ++  MaxLessN xs n

//Start = MaxLessN [[1..10],[5,10..50]] 36


// Write a function that returns the list of prime number between two Integer


/*
Write a function that takes two vectors, represented as list, and returns their dot product.
*/

DotProduct :: [Int] [Int] -> Int
DotProduct [] [] = 0
DotProduct [x:xs] [y:ys] = x*y + DotProduct xs ys

//Start = DotProduct [4,5,6] [6,4,8]

/*
Given two list of different lengths, write a function that sums the elements of the same indices
, into a different list
*/

ListSum :: [Int] [Int] -> [Int]
ListSum [] _ = []
ListSum _ [] = []
ListSum [x:xs] [y:ys] = [x + y] ++ ListSum xs ys

//Start = ListSum [1..5] [1..3]

// Write a function that takes a list of Int and returns a list where each number of the
// list is subsituted with the number of it's divisors.

DivisorAux :: Int Int [Int] -> [Int]
DivisorAux inc stop accum
| inc > stop = accum
| stop rem inc == 0 = [inc] ++ accum ++  DivisorAux (inc+1) stop accum
= DivisorAux (inc+1) stop accum

//Start = DivisorAux 3 33 []

Divisor :: Int -> [Int]
Divisor num = DivisorAux 1 num []

//Start = Divisor 15

SubsDiv :: [Int] -> [Int]
SubsDiv [] = []
SubsDiv [x:xs] = [length(Divisor x)] ++ SubsDiv xs

//Start = SubsDiv [12,6,11,55]

/* Given three numbers, determine if any of the two numbers addition's sum of digits produces a lucky number.
A lucky number is a number whose sum of digits is divisible by 3 */




isThereLucky :: Int Int Int -> Bool
isThereLucky x y z
|((x + y) rem 3) == 0 = True
|((y + z) rem 3) == 0 = True
|((x + z) rem 3) == 0 = True
= False

//Start = isThereLucky (5) (3) (2)

/* Student recently learned Fibonacci numbers, but he got bored of the problems.
    So, he decided to try his own new sequence:
        f(0) = a
        f(1) = b
        f(n) = f(n-1) ^ f(n-2) where ^ denotes bitxor operation (Hint: bitxor operator can be found in the documentation)
    Given a, b, and n, make a function to help him calculate the nth number in his sequence.
    For example:
    



*/    

diffFibonacchi :: Int Int Int -> Int 
diffFibonacchi a b 0 = a
diffFibonacchi a b 1 = b
diffFibonacchi a b n 
| n rem 2 == 0 = diffFibonacchi a ((bitxor) a b) (n-1)
= diffFibonacchi ((bitxor) a b) b (n-1)


//Start = diffFibonacchi 86 77 15 // = 86
//Start = diffFibonacchi 9 7 8 // = 14
//Start = diffFibonacchi 86 77 14 // = 27

//Start = ( bitxor ) 2 -7

/* 1. Given a list of numbers return the index of the number that including that
number the sum of all previous numbers equals 0.

Example:
equalsZero [1,2,3,-3,-2,-1,3,4,1,-1] = 5
which is the index of -1. The sum of [1,2,3,-3,-2,-1] is zero.
If there is no such number return -1 */

equalsZeroAux :: [Int] Int -> [Int]
equalsZeroAux [] num  = []
equalsZeroAux [x:xs] num
|(num + x) <> 0 = [x] ++ equalsZeroAux xs (num + x)
= [x]

//Start = equalsZeroAux [1,2,3,-2,-1,-3] 0 

equalsZero :: [Int] -> Int
equalsZero list 
| length list == length (equalsZeroAux list 0 ) && sum list == 0 = (length (equalsZeroAux list 0 )) - 1
| length list == length (equalsZeroAux list 0 ) && sum list <> 0 = -1
= (length (equalsZeroAux list 0 )) - 1

//Start = equalsZero [1,2,3,-3,-2,-1,3,4,1,-1]
//Start = equalsZero [1,2,3,-2,-1,-3]
//Start = equalsZero []
//Start = equalsZero [1,2,3,-2,-1,3,4,1,-1]

/*
Given 2 dimentional list of Reals, sum all the numbers inside sublist and only keep the numbers if the fraction part is equal to zero.



Example [[1.2, 1.3, 1.5], [3.53, 53.42, 53.21]] => [4.0, 110.16] => [4]
Hint: You can use map function (not required)    */



pt3 :: [[Real]] -> [Int]
pt3 [] = []
pt3 [x:xs] 
| (toInt(sum x))*10 == toInt((sum x) * 10.0) = [ toInt(sum x)] ++ pt3 xs
= pt3 xs



//Start = pt3 [[1.2, 1.3, 1.5], [3.53, 53.42, 53.21]] // [4]
//Start = pt3 [[1.2, 1.8, 3.9], [4.8, 7.9, 6.7], [6.9]] // []



 

















 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 