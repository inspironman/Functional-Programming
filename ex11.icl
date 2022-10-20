module ex11
import StdEnv

// Exercises

// 1. Write a recursive function that computes the n-th multiple of an x.
f1 :: Int Int -> Int
f1 x 0 = 0
f1 x y = x + (f1 x (y-1))

//Start = f1 2 5

// 2. Add 2 to every odd number of a list, and subtract 2 from every even number.
f2 :: [Int] -> [Int]
//f2 list = [ x-2 \\ x <- list | isEven x ] ++ [ y+2 \\ y <- list | isOdd y ]
f2 [] = []
f2 [x:xs] 
| isEven x = [x-2] ++ f2 xs
= [x+2] ++ f2 xs

//Start = f2 [4,5,64,34,5,3,2]

// 3. Compute the triple of the negative elements of a list up to the first positive number.
f3 :: [Int] -> [Int]
f3 list = takeWhile ((>) 0)[x*3 \\ x <- list]


//Start = f3 [-1,-6,-3,1,-5,6]

// 4. Write a function that keeps the non-zero elements of a list and then multiply by 2 every element.
f4 :: [Int] -> [Int]
f4 x = filter ((<>)0 )(map ((*) 2) x)

//Start = f4 [-1,-6,-3,0,1,3,0,-5,6]

// 5. Write a function for the square, the cube, and so on up to the n-th power of a number,
// so that increasing powers of a number are obtained in a list.

f5 :: Int Int -> [Int]
f5 x n = [ x^b \\ b <- [2..n] ]

//Start = f5 5 13

// 6. Replicate n>0 times a list.
f6 :: Int [Int] -> [[Int]]
f6 n list = [ x \\ x <- [list] , y <- [1..n] ]

//Start = f6 3 [3,4,5,1]

//=========================******************************==============================
// 7. Insert 0 at the middle of each sublist.
f7 :: [[Int]] -> [[Int]]
f7 x =  map (\x = take ((length x)/2) x ++ [0] ++ drop ((length x)/2) x) x

//Start = f7 [[1,2,3],[6,4,5],[6,7,8,9]]

// 8. Extract the elements smaller then the head element of a list.
f8 :: [Int] -> [Int]
f8 list = [x \\ x <- list | hd list > x ]

//Start = f8 [5,2,6,7,9,11,23,3]

// 9. Eliminate in a list the sublists that are longer then 10.

f9 :: [[Int]] -> [[Int]]
f9 list = [ x \\ x <- list | length x < 10 ]

//Start = f9 [[1..7],[1..12],[1,1,2,2,4,3,2,2,21,3,4],[2,4,26]] 

// 10. Compute the greatest common divisor in a recursive function.

f10 :: Int Int -> Int
f10 x y
| x > y = f10 (x-y) y
| x < y = f10 x (y-x)
= x

//Start = f10 10 56


// 11. Compute the Euler number aproximation in n steps: e = 1/0! + 1/1! + 1/2! + 1/3! + ... 

facaux :: Int -> Int
facaux 0 = 1
facaux num = num * facaux (num-1)

f11 :: Int -> Real
f11 n =  sum [ (toReal (x))/(toReal (facaux y)) \\ x <- [1..n] , y <- [0..n]] 


//Start = f11 1000











