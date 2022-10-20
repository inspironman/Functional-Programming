module midsem4
import StdEnv

/* 1. Parasitic Number

 A Parasitic number (in base 10) is a positive number which can be multiplied 
 by a certain n by moving the rightmost digit of its decimal representation 
 to the front.
 e.g. 102564 × 4 = 410256
 Given a positive integer number and n, write a function to determine whether 
 it is a Parasitic number or not.
*/
digit :: Int -> [Int]
digit num 
| num > 9 = [ num rem 10 : digit (num/10)]
= [num]

//Start = digit 16535
//Start = [5] ++ init (reverse (digit 142857))
//[5,1,4,2,8,5]
//Start = reverse (digit (142857*5)) 
//[7,1,4,2,8,5]

parasitic :: Int Int -> Bool
parasitic num num2 = [num2] ++ init (reverse (digit num)) == reverse (digit (num*num2))  

//Start = parasitic 102564 4 // True
//Start = parasitic 142857 5 // True
//Start = parasitic 714285 8 // False
//Start = parasitic 105263157894736842 2 // True

/* 2. Double Ones

 Given a list of integers, write a function which will keep only the numbers
 that contain at least two '1' digits. For example:
 [1,2,21,121,11,234131,111111,123,0,334] -> [121,11,234131,111111]
*/

checkOne :: Int -> Bool
checkOne num
| num > 10 = length (filter ((==)1) (digit num)) >= 2
= False 

//Start = checkOne 15641

doubleOne :: [Int] -> [Int]
doubleOne list = [ x \\ x <- list | checkOne x ]

//Start = doubleOne [1,2,21,121,11,234131,111111,123,0,334] // [121,11,234131,111111]
//Start = doubleOne [12,1,11,33] // [11]
//Start = doubleOne [11,111,21] // [11,111]
//Start = doubleOne [] // []
//Start = doubleOne [21,3,1] // []

/* 3. Multiples

 Given an n>0 integer value, write a function that creates the double, the triple
 and so on n-th multiple of the number.
*/

multiple :: Int -> [Int]
multiple num = [ num*y \\ y <- [2..num] ]

//Start = multiple 5 // [10,15,20,25]
//Start = multiple 2 // [4]
//Start = multiple 1 // []

/* 4. List difference
 
 Given two lists (A and B) containing sublists of integer numbers, 
 both A and B are of the same length,
 for every sublist in A and B, return the difference of the two sublists.  

 The difference is defined as follows:  
 The List L1-L2 consists of elements that are in L1 but not in L2. 
 For example if L1=[1,2,3] and L2=[3,5], then L1-L2=[1,2].
*/

f4Aux :: [Int] [Int] -> [Int]
f4Aux l1 l2 = [ x \\ x <- l1 | isMember x l2 == False ]

//Start = f4Aux [1,2,3,4,5,3,22] [2,1,3,5,21]

difference :: [[Int]] [[Int]] -> [[Int]]
difference l1 l2 = [ f4Aux x y \\ x <- l1 & y <- l2 ]


//Start = difference [[1..5]] [[4..7]] // [[1,2,3]]
//Start = difference [[1..10] , [10..15] , [1..4]] [[1..10] , [11..20] , [5]] // [[],[10],[1,2,3,4]]
//Start = difference [] [] // [] 


/* 5. Replace middle

 Given a list of lists of integers and an integer, write a function that replaces 
 the middle element with the given integer in every sublist. 
*/

repMid :: [[Int]] Int -> [[Int]]
repMid biglist num = [ init (take ((length list + 2)/2) list) ++ [num] ++ drop ((length list + 2)/2) list \\ list <- biglist ]

//Start = repMid [[1,2,3],[1..4]] 10 // [[1,10,3],[1,2,10,4]]
//Start = repMid [[1..6], [9,8..1], [(-1),(-2)..(-10)]] 5 
          // [[1,2,3,5,5,6],[9,8,7,6,5,4,3,2,1],[-1,-2,-3,-4,-5,5,-7,-8,-9,-10]]
          // [[1,2,3,5,5,6],[9,8,7,6,5,4,3,2,1],[-1,-2,-3,-4,-5,5,-7,-8,-9,-10]]
//Start = repMid [[1,3],[]] 5 // [[1,5],[5]]


/* 6. Primes7

 Given a list of numbers, keep only the prime numbers that end with the digit 7
*/

isPrime :: Int -> Bool
isPrime num = length [ x \\ x <- [1..num] | num rem x == 0] == 2

primes7 :: [Int] -> [Int]
primes7 list = [ x \\ x <- list | (isPrime x) && (x rem 10 == 7) ]

//Start = primes7 [1..10] // [7]
//Start = primes7 [1..100] // [7,17,37,47,67,97]
//Start = primes7 [1..6] // []

/* 7. Property check

 Given a list of tuples, write a function to determine
 whether all of the tuples inside of the list hold the (Even, Odd) property.
 [(2,1),(2,3),(4,1)] = True
*/	

holdsTrue :: [(Int,Int)] -> Bool
holdsTrue [] = False
holdsTrue list = and [ isEven (fst x) &&  isOdd (snd x)  \\  x <- list ]

//Start = holdsTrue [(2,1),(2,3),(4,1)] // True
//Start = holdsTrue [(1,3),(2,3),(3,4)] // False
//Start = holdsTrue [] // False

 
/* 8. Super Digit

 We define super digit of an integer x using the following rules.
 If x has only 1 digit, then its super digit is x.
 Otherwise, the super digit of x is equal to the super digit of the digit-sum of x.
 Here, the digit-sum of a number is defined as the sum of its digits.

 E.g  : super_digit(9875) = super_digit(9+8+7+5) 
                          = super_digit(29) 
                          = super_digit(2+9)
                          = super_digit(11)
                          = super_digit(1+1)
                          = super_digit(2)
                          = 2

 Given a list of integers, return a list containing the super digit
 of every number in the list.  
*/


SdigitAux :: Int -> Int
SdigitAux 1 = 1
SdigitAux num
| num > 10 =  SdigitAux (sum (digit num))
= num 

//Start = SdigitAux 9875

Sdigit :: [Int] -> [Int]
Sdigit list = [ SdigitAux x \\ x <- list ]

//Start = Sdigit [148148148 , 9875 ] // [3,2]
//Start = Sdigit [884555 , 456 , 2351 , 21587 , 88 ] // [8,6,2,5,7]
//Start = Sdigit [] // [] 

/* 9. Powers 
 Given a list of integers and an integer, write a function which returns a list 
 which only contains the powers of the integer.
*/

powerList :: [Int] Int -> [Int]
powerList list num = [ x \\ x <- list , y <- [0..length list] | num^y == x ]


//Start = powerList [2,4,8,16,32,33,55] 2 // [2,4,8,16,32]
//Start = powerList [] 3 // []
//Start = powerList [1..10] 3 // [1,3,9]
//Start = powerList [-1,-2,4,8] 4 // [4]

/* 10. Twin primes
 
 Twin primes is a pair of primes, such that it contains a prime number that is either 
 2 less or 2 more than the pair prime number.
 For example, (41, 43) is a twin prime pair.
 Given a range of numbers left..right write a function that returns the count of 
 twin primes within the range.

 E.g: between 1 and 50 there are 6 pairs of twin prime numbers:
 [(3,5),(5,7),(11,13),(17,19),(29,31),(41,43)].
*/
 
twinPrimes :: Int Int -> Int
twinPrimes num1 num2 = length [ (x,y) \\ x <- [num1..num2] , y <- [num1..num2] | (isPrime x && isPrime y) && (x <> y) && ( (x-y) == 2 ) ] 
 


//Start = twinPrimes 1 50 // 6
//Start = twinPrimes 1 1000 // 35
//Start = twinPrimes 0 2 // 0
//Start = twinPrimes 0 -5 // 0

//[(\x y | isEven x = isOdd y = isEven y) index a \\ a <- list & index <-[0..]]

dif2 :: [Int] -> [Int]
dif2 list = 

Start = dif2 [1,2,3,4]






























