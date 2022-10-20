module ex4

import StdEnv

// Compute the nth Fibonacci number - try more versions
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

//Start = fib 8

fib1 :: Int -> (Int, Int)
fib1 0 = (1,1)
fib1 1 = (1,1)
fib1 n = (b,a+b)
where
	(a,b) = fib1 (n-1)

//Start = fib1 8
fib2 :: Int Int Int -> Int
fib2 a b 0 = a
fib2 a b c = fib2 b (a+b) (c-1)

//Start = fib2 1 1 10

fib3 :: Int -> Int
fib3 n = fibAux n 1 1

fibAux 0 a b         = a
fibAux i a b | i > 0 = fibAux (i-1) b (a+b)

//Start = fib3 8
 
// Exercises
  
// 1. Compute the product of the elements of a list
product :: [Int] -> Int
product [] = 1
product [x:xs] = x * product xs


//Start = product [1..5] // 120

// 2. delete the elements equal to 5
not_five :: [Int] -> [Int]
not_five [] = []
not_five [x:xs]
| x == 5 = not_five xs
= [x: not_five xs]

//Start = not_five [5,4,5,4,3]  // [4,4,3]

not_five1 :: [Int] -> [Int]
not_five1 [] = []
not_five1 [5:xs] = not_five1 xs
not_five1 [x:xs] = [x:not_five xs]

//Start = not_five1 [5,4,5,4,3]

// 3. Delete an element n from a list
del :: Int [Int] -> [Int]
del x [] = []
del n [x:xs] 
| x == n = del n xs
= [x:del n xs]


//Start = del 5 [1, 5, 6, 7, 5, 8, 5] // [1, 6, 7, 8]


// 4. write a funtion with the patterns depending on the parameter:
// if the param is [] then is equal to 20, if is a two element list starting with 4 then is 30
// if is a two element list ending with 5 then is 40, in all other cases is 50, 
// the order of the patterns is important
gp :: [Int] -> Int
gp [] = 20
gp [4,_] = 30
gp [_,5] = 40
gp [_,_] = 50

//Start = gp [4, 5] // 30


// 5. write a funtion which returns true if a is divisible by b
div_by :: Int Int -> Bool
div_by x y =  x rem y == 0
 


//Start = div_by 16 4    // True


// 6. write a funtion which returns true if a is divisible by b or vice versa
div_any :: Int Int -> Bool
div_any x y = x rem y == 0 || y rem x == 0


//Start = div_any 4 16     // True


// 7. sumsq n returns 1*1 + 2*2 + ... + n*n - with a pattern for 0
sumsq :: Int -> Int
sumsq 1 = 1
sumsq x = x*x + sumsq (x-1)

//Start = sumsq 3 // 14


//version 2. - without pattern for 0

sums :: Int -> Int
sums n
| n == 0 = 0
| otherwise = n*n + sums (n-1)


//Start = sums 3
  

p :: Int -> [Int]
p 0 = []
p x = digits x []

//Start = p 12345  

digits :: Int [Int] -> [Int]
digits 0 x = x
digits n x = digits (n/10) [n rem 10 : x ]

Start = digits 12321 []

// 8. check if a number is palindrom e.g.12321

pali :: Int -> Bool
pali x = y == reverse y
where y = p x 

//Start = pali 12321 // True
 

//9. filter the elements that are satisfying a condition



filter` :: (Int -> Bool) [Int] -> [Int]
filter` f [] = []
filter` f [x:xs] 
| f x = [x: filter` f xs]
= filter` f xs

//Start = filter` ((<>) 5) [1, 5, 6, 7, 5, 8, 5]  // [1, 6, 7, 8] 
//Start = filter` isEven [1,2,3,4]