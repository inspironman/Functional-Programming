module ex11_solve
import StdEnv

// Exercises

// 1. Write a recursive function that computes the n-th multiple of an x.
f1 :: Int Int -> Int
f1 0 x = 0
f1 n x = x + f1 (n-1) x

//Start = f1 5 10


// 2. Add 2 to every odd number of a list, and subtract 2 from every even number.
f2 :: [Int] -> [Int]
f2 [] = []
f2 [x:xs]
| isOdd x = [x+2 : f2 xs]
= [x-2 : f2 xs]

//Start = f2 [1..5]


g :: Int -> Int
g x 
| isOdd x = x+2
= x-2

f22 :: [Int] -> [Int]
f22 x = map g x

//Start = f22 [1..5]


// 3. Compute the triple of the negative elements of a list up to the first positive number.
f3 :: [Int] -> [Int]
f3 x = map ((*)3) (takeWhile ((>) 0) x)

//Start = f3 [-1,-3,-5,-5,2,-4,-5]


// 4. Write a function that keeps the non-zero elements of a list and then multiply by 2 every element.
f4 :: [Int] -> [Int]
f4 x = map ((*)2) (filter ((<>) 0) x)

//Start = f4 [1,2,3,0,5,0,6,0,0,0,0] 


// 5. Write a function for the square, the cube, and so on up to the n-th power of a number,
// so that increasing powers of a number are obtained in a list.
f5 :: Int Int -> [Int]
f5 1 x = []
f5 n x = f5 (n-1) x ++ [x^n]

//Start = f5 5 2


// 6. Replicate n>0 times a list.
f6 :: Int [Int] -> [[Int]]
f6 0 x = []
f6 n x = [x : f6 (n-1) x]

//Start = f6 3 [1..10]


// 7. Insert 0 at the middle of each sublist.
f7 :: [[Int]] -> [[Int]]
f7 x = map (\x = take ((length x)/2) x ++ [0] ++ drop ((length x)/2) x) x

//Start = f7 [[1..10], [1..11], [], [1], [1,2]]


// 8. Extract the elements smaller then the head element of a list.
f8 :: [Int] -> [Int]
f8 x = filter ((>)(hd x)) x

//Start = f8 [5,1,2,3,4,5,6,7,8]


// 9. Eliminate in a list the sublists that are longer then 10.
f9 :: [[Int]] -> [[Int]]
f9 x = filter (\x = length x <=10) x

//Start = f9 [[1..10], [1..11], [1..5], []]


// 10. Compute the greatest common divisor in a recursive function.
f10 :: Int Int -> Int
f10 a b 
| a > b = f10 (a-b) b
| b > a = f10 a (b-a)
= a

//Start = f10 24 12


// 11. Compute the Euler number aproximation in n steps: e = 1/0! + 1/1! + 1/2! + 1/3! + ... 
//f11 :: Int -> Real

// Start = f11 1000