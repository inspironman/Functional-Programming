module midsem2
import StdEnv

// 1. Write a function that will return the second to last digit in a number. 
//    Return 0 if there is no second digit.

digit :: Int -> [Int] 
digit num 
| num > 9 = [num rem 10 : digit (num/10)] 
= [num]

//Start = digit 1345

SecLast :: Int -> Int
SecLast num 
| length (digit num) >= 2 = (digit num) !! 1
= 0


//Start = SecLast 1234 //3

//Start = SecLast 5 //0

//Start = f1 -5564 //6

// 2. Write a function that will subtract numbers in a list from the first one. 
//    Your solution must use 'foldr' or 'foldl'.

f2 :: [Int] -> Int
f2 [x:xs] = foldl (-) x xs

// Return 0 for an empty list.

//Start = f2 [10,1,2,3] //4

//Start = f2 [1,2,3,4] //-8

//Start = f2 [1000,500,250,125] //125

//Start = f2 [] //0

// 3. Write a function that returns all prime divisors of a number. e.g. f3 36 = [1,2,3]


isPrime :: Int -> Bool
isPrime num = length [ x \\ x <- [1..num] | num rem x == 0] == 2

f3  :: Int -> [Int]
f3 num = [ x \\ x <- [1..num] | num rem x == 0 && isPrime x ]

//Start = f3 26

// 4. Write a function that reverses tuples from a list if the tuple members sum up 
//    to an even number.

f4 :: [(Int,Int)] -> [(Int,Int)]
f4  [] = []
f4 [ (a,b):xs ]
| isEven (a+b) = [(b,a) : f4 xs ]
= [(a,b) : f4 xs ]

//Start = f4 [(1,2),(3,4),(5,7)] //[(1,2),(3,4),(7,5)]

//Start = f4 [(-1,3),(12,1),(0,0),(-4,-2)] //[(3,-1),(12,1),(0,0),(-2,-4)]

//Start = f4 [] //[]

// 5. Write a function that takes every number in a list and generates a sublist of 
//    its first 5 multiples. Your solution must use 'map'.

f5Aux :: Int -> [Int]
f5Aux num = [ num*x \\ x <- [1..5] ]

//Start = f5Aux 6 

f5 :: [Int] -> [[Int]]
f5 list = map (f5Aux) list


//Start = f5 [1..3] //[[1,2,3,4,5],[2,4,6,8,10],[3,6,9,12,15]]

//Start = f5 [4,~3,5,~6] //[[4,8,12,16,20],[-3,-6,-9,-12,-15],[5,10,15,20,25],[-6,-12,-18,-24,-30]]

//Start = f5 [] //[]

// 6. Given an integer n, find the minimal k such that

// k = m! (where m! = 1 * 2 * ... * m) for some integer m; k >= n.

// In other words, find the smallest factorial which is not less than n.

// example: leastfactorial 17 = 24.  because 17 < 24 = 4! = 1 * 2 * 3 * 4, while 3! = 1 * 2 * 3 = 6 < 17

factorial :: Int -> Int
factorial 0 = 1
factorial num = num * factorial (num-1)

faclist :: Int -> Int
faclist 1 = 1
faclist num = (filter ((<) num) [ factorial x \\ x <- [1..num]]) !! 0

//Start = faclist 17 // 24

//Start =  faclist 1 // 1

//Start =  faclist 5 // 6

//Start =  faclist 25 // 120

// 7. Write a function that checks if a list of numbers is odd,even,odd,even...

// e.g. for [1,2,3,4,6] it is false because 4 is even, but 6 is not odd.

OddEven :: [Int] -> Bool
OddEven [] = False 
OddEven list = and [ ( isEven x && isEven y ) || ( isOdd x && isOdd y) \\ x <- list & y <- [1..] ]

//Start =  OddEven [1..10] //True

//Start =  OddEven [1,2,3] //True

//Start =  OddEven [2,3,4] //False

//Start =  OddEven [1,3,4,5] //False

//Start =  OddEven [1,2,3,4,6,7] //False

//Start =  OddEven [] //False

// 8. Write a function that removes consecutive duplicates in a list.

remDup :: [Int] -> [Int]
remDup [] = []
remDup [x] = [x]
remDup [x,y:xs] 
| x == y  = [x : remDup xs]
= [x : remDup [y:xs] ]

//Start = remDup [4,5,6,6,8,2,2,2,4,0,0,0,7,0,5,0,0,4]

f8 :: [Int] -> [Int]
f8 [] = []
f8 [a] = [a]
f8 [x:xs:xss]
|(x==xs) = f8 (dropWhile ((==)x) xss)
= [x] ++ f8 [xs:xss]

//Start = f8 [4,5,6,6,8,2,2,2,4,0,0,0,7,0,5,0,0,4] //[4,5,8,4,7,0,5,4]

//Start = f8 [1,0,0,2,0,3,3,0,6,7,0,7,7] //[1,2,0,0,6,7,0]

//Start = f8 [2,0,0,6,7,5,0,8,0,5,0,0,0] //[2,6,7,5,0,8,0,5] 

// 9. Write a function that takes a tuple of three lists and generates a list of triple tuples.

// The triple tuple is only generated if the i-th member of the first list multiplied by the

// i-th member of the second list equals the i-th member of the third list.

// e.g. for ([1,2,3,4,5],[2,4,6,8,10],[2,8,17,32,45]) the result is [(1,2,2),(2,4,8),(4,8,32)]

tritup :: ([Int],[Int],[Int]) -> [(Int,Int,Int)]
tritup list = [ (x,y,z) \\ x <- fst3 list & y <- snd3 list & z <- thd3 list ]

//Start = tritup ([2,2,2,2,2,2],[1,2,3,4,5,6,7,8],[2,4,6,6,10])//[(2,1,2),(2,2,4),(2,3,6),(2,5,10)]

//Start = tritup ([1,2,3,4,5],[2,4,6,8,10],[2,8,1,32,45])//[(1,2,2),(2,4,8),(4,8,32)]

//Start = tritup ([1,0,1,0,1,0],[3,4,5,6,8],[3,0,5,0,0])//[(1,3,3),(0,4,0),(1,5,5),(0,6,0)]


// 10. A Mersenne Prime is a prime number that is 1 less than a power of 2. 
//     example: 7 = (2^3) - 1 = 8-1


MPrimeAux :: Int -> [Int]
MPrimeAux num = [ (2^x)-1 \\ x <- [1..] ]

MPrime :: Int -> Bool
MPrime 1 = False
MPrime num = isPrime num && isMember num (MPrimeAux num)

//Start = MPrime 21
//Start = MPrime (~235)
//Start = MPrime 7
//Start = MPrime 2147483647 // CRASHING
Start = MPrime 11 // CRASHING

















