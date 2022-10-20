module ex5
import StdEnv

// 1. write a recursive function that computes n at power k (n, k positive numbers)
power :: Int Int -> Int
power x 0 = 1
power x y = x* power x (y-1) 

//Start = power 2 5 // 32


// 2. add 3 to every element of a list
f1 :: [Int] -> [Int]
f1 [] = [3]
f1 [x:xs] = [x+3] ++ f1 xs

//Start = f1 [1,5,3,1,6]  // [4,8,6,4,9]  


// 3. compute the double of the positive elements of a list [1, 2, -2, 3, -4] -> [2, 4, 6]
// hints: first filter it then use map 
f2 :: [Int] -> [Int]
f2 [] = []
f2 [x:xs]
| x > 0 = [2*x] ++ f2 xs
= f2 xs

//Start = f2 [1, 2, -2, 3, -4] // [2, 4, 6]


// 4. write a function that keeps the integers of a list up to the first 0 encounterred 
// and then divides by 2 every element [1, 2, -2, 3, 0, -4] -> [0, 1, -1, 1]
// hints: use takeWhile then map
f3 :: [Int] -> [Int]
f3 [x:xs]
| x == 0 = []
| x > 0 = [x/2] ++ f3 xs
| x < 0 = [x/2] ++ f3 xs

//Start = f3 [1, 2, -2, 3, 0, -4] // [0, 1, -1, 1]


// 5. write a function for the square of every element of a list and sublists
// [[1,2],[3,4,5,6],[7,8]]  -> [[1,4],[9,16,25,36],[49,64]]  
// hint: map in map

f4aux :: [Int] -> [Int]
f4aux [] = []
f4aux [x:xs] = [x*x] ++ f4aux xs 

f4 :: [[Int]] -> [[Int]]
f4 [] = []
f4 [x:xs] = [f4aux x] ++ f4 xs

//Start = f4 [[1,2],[3,4,5,6],[7,8]] // [[1,4],[9,16,25,36],[49,64]]


// 6. Replicate n>0 times the element of a list e.g. n=3 [3..6] ->
// [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]

f5aux :: Int Int -> [Int]
f5aux 0 x = []
f5aux n x = [x : f5aux (n-1) x]

//Start = f5aux 3 5

f5 :: Int [Int] -> [[Int]]
f5 n [] = []
f5 n [x:xs] = [f5aux n x : f5 n xs]

//Start = f5 3 [3..6]


// 7. insert 0 at the beginning of each sublist
// [[1,2], [3,4,5], [6,7]] -> [[0,1,2], [0,3,4,5], [0,6,7]]

f6aux :: [Int] -> [Int]
f6aux [] = []
f6aux [x:xs] = [0,x:xs]

//Start = f6aux [1,2,3,6,4,5]

f6 :: [[Int]] -> [[Int]]
f6 [] = []
f6 [x:xs] = [f6aux x : f6 xs]

//Start =  f6 [[1,2], [3,4,5], [6,7]]


// 8. filter the elements smaller then n, e.g. n=3 [1,5,3,2,1,6,4,3,2,1] -> [1,2,1,2,1]

f7 :: Int [Int] -> [Int]
f7 n [] = []
f7 n [x:xs]
| x > n = [x] ++ f7 n xs
= f7 n xs

//Start = f7 3 [1,5,3,2,1,6,4,3,2,1] 


// 9. using notempty eliminate the empty lists: 
// [[1,2,3],[],[3,4,5],[2,2],[],[],[]] -> [[1,2,3], [3,4,5], [2,2]]

notempty :: [Int] -> Bool
notempty x = not (x == [])

f8 :: [[Int]] -> [[Int]]
f8 [] = []
f8 [x:xs]
| notempty x = [ x : f8 xs]
= f8 xs

//Start = f8 [[1,2,3],[],[3,4,5],[2,2],[],[],[]]


// 10. compute the sum of the sublist using foldr [[1,2,3], [3,4,5], [2,2]] -> [6, 12, 4]

f9 :: [[Int]] -> [Int]
f9 [] = []
f9 [x:xs] = [sum x : f9 xs]

//Start = f9 [[1,2,3], [3,4,5], [2,2]]


// 11. (bonus point) rewrite map using foldr

inc :: Int -> Int
inc 0 = 1
inc x = x + 1

//Start = inc 15

//mymap :: (a -> b) [a] -> [b]
//mymap inc [] = [1]
//mymap inc [x:xs] = [inc x] ++ mymap inc xs



//Start = mymap inc [1..10]

f10 :: Int [Int] -> [Int]
f10 n [] = []
f10 n [x:xs]
| n rem x == 0 = [x] ++ f10 n xs
= f10 n xs

//Start = f10 496 [1..495]

mainaux :: Int -> Bool
mainaux 1 = True
mainaux num 
| sum ( f10 num [1..num-1] ) == num = True
= False 

//Start = mainaux 496

perfectnum :: [Int] -> [Int]
perfectnum [] = []
perfectnum [x:xs]
| mainaux x = [x] ++ perfectnum xs
= perfectnum xs

//Start = perfectnum [1..29]

done :: Int -> Int
done num = length (perfectnum [2..num])

Start = done 1000



