module check3
import StdEnv

// Q-Sort

qsort :: [a] -> [a] | Ord a
qsort [] = []
qsort [c:xs] = qsort [ x \\ x <- xs | x < c] ++ [c] ++ qsort [x \\ x <- xs | x > c]

//Start = qsort [8,4,5,6,3]

merge :: [a] [a] -> [a] | Ord a 
merge [a] [] = [a]
merge [] [a] = [a]
merge [] [] = []
merge [x:xs] [y:ys]
| x < y = [x] ++ merge xs [y:ys]
= [y] ++ merge [x:xs] ys

//Start = merge [8,4,5,6,3] [1,7,2,10]

msort :: [a] -> [a] | Ord a
msort [] = [] 
msort xs
| length xs == 1 = xs
= merge (msort flist) (msort slist)
where 
       flist = take half xs
       slist = drop half xs
       half = (length xs)/2
       
//Start = msort [1,7,2,8,4,5,6,3,10]

/*
If the sum of cubes of each digit of the number is equal to the number itself, then the
number itself then the number is called an armstrong number.
153 = 1^3 + 5^3 + 3^3
Given a positive integer number, write a function to determine whether it is an armstrong 
or not 
*/

digits :: Int -> [Int]
digits num
| num > 9 = [num rem 10] ++ digits (num/10)
= [num]

//Start = digits 123

armstrongaux :: Int -> [Int]
armstrongaux num = [x*x*x \\ x <- digits num ]


//Start = armstrongaux 135

armstrong :: Int -> Bool
armstrong num = num == sum(armstrongaux num)


//Start = armstrong 370

/*
Given a list of integers, replace every element in the list with its number of 
occurencies in the list
*/

occNum :: [Int] -> [Int]
occNum list = [length (filter ((==) a) list) \\ a<-list ]
//removeDup [(filter ((==) a) list) \\ a<-list ]
//occNum list = map( \x = length (filter ((==) x) list) list

//Start = occNum [1,2,1,1,34,44,5,55,56,55,64,3]

/*
Given a list of numbers, convert the list in a way such that;
the difference between two consicutive elements is always 2 
you may have to add numbers in order to achieve that 
e.g: [1,5,8] = [1,3,5,7,9]
*/

gap2 :: [Int] -> [Int]
gap2 list
|isOdd (minList list) = [minList list,((minList list) + 2) .. ((maxList list) + 1)]
= [minList list,((minList list) + 2) .. ((maxList list))]

//Start = gap2 [1,3,6,8]

/*
Not Palindrome
Given a list of lists of Integers. 
Write a function that gets rid of Palindrome numbers.
A palindrome number is a number that can be read from left to right or from right to left 
151 is a palindrome number.
*/

not_palin :: Int -> Bool
not_palin num
| length (digits num) == 1 = True
= reverse (digits num) <> digits num

//Start = not_palin 151

getRidPal :: [[Int]] -> [[Int]]
getRidPal list = [ filter (not_palin) x \\ x <- list ]

//Start = getRidPal [[1,2,1111],[151,22,3455]] 

/*
Given a list of Integers, write a function which removes the Prime Numbers from the list.
There will be no negative integers and consider the number 1 not a prime.
*/
Prime :: Int Int -> Int
Prime 0 m = 0
Prime n m
| (m rem n == 0) = 1 + Prime (n-1) m
= Prime (n-1) m

primechecker :: Int -> Bool
primechecker x
| (Prime x x) == 2 = True
=  False
//Start = primechecker 4 // []


remPrime :: [Int] -> [Int]
remPrime list = [x \\ x <- list | not (primechecker x)]

//Start = remPrime [1,2,3,6,7,8,9]

/*
Implement the Function zipWith that takes a function and two lists, and combine them 
in such a way that elements that are in the same position get the function applied to 
them 

e.g. zipWith addTwoNumber [1,2,3] [5,6,7] = [1+5,2+6,3+7] = [6,8,10]
*/

//add :: Int Int -> Int
//prod :: Int Int -> Int

zipWith ::  [Int] [Int] -> [Int]
zipWith ls1 ls2 = [ a + b \\   a <- ls1 & b <- ls2 ]

//Start = zipWith [1,2,3,4] [5,6,7,8]


/*
Given a Positive number greater than 1, return how many iteration does it take
for that number to fail down to "2" if we keep applying the collatz equation on it.
collatz conjecture equation:

if number is even -> x/2
if number is odd -> 3x+1

e.g. input 10
10 -> 5 -> 16 -> 4 -> 2

output 4 recursive calls 
*/





/*
Given a lists of lists count how many of the sublist are good lists.
A list is good if it's empty or its list number is odd , 2nd is even, 3rd is odd ,
4th is even and so on.
e.g. = [[],[1,2,3,4],[8,3,4],[9],[3,4,4]] your function should return 
3 as only [], [1,2,3,4] and [9] are "good".
*/


goodCheck :: [Int] -> Bool
goodCheck list = and [ ( \x y | (isOdd x && isOdd y) || (isEven x && isEven y) = True = False ) a b \\ a <- list & b <- [1..] ]

//Start = goodCheck [5,2,3,4,6]

goodlist :: [[Int]] -> Int
goodlist list = length [ a \\ a<-list | goodCheck a ]

//Start = goodlist [[],[1,2,3,4],[8,3,4],[9],[3,4,4]]


/*
Given a list of lists of integers, Write a function which returns a list of 
symmetrical lists if the sum of the sublist is greater than 10
*/

symaux :: [Int] -> Bool
symaux list = sum list > 10

//Start = symaux [1..3]

symlist :: [[Int]] -> [[Int]]
symlist list = [ x  ++ reverse x\\ x <- list | symaux x]

//Start = symlist [[1..10],[6..10],[1..3],[2,5,1]] 

/*
Given a list of triple tuple consisting of two integer value and a list of integers 
(left,right,[Int]) , for every tuple return only the elements from the list which 
positions are inside the interval [left..right]
Assume that the index all are valid.
*/

getelement :: [Int] Int Int -> [Int]
getelement list x y = [ a \\ a<- list & b <- [0..] | isMember b [x..y] ]

//Start = getelement [3,6,8,9,6,4,8] 2 6

eleInt :: [(Int, Int,[Int])] -> [[Int]]
eleInt list = [ getelement c a b \\ (a,b,c) <- list ]

Start = eleInt [( 2 , 5 , [1..10])]





















































































































