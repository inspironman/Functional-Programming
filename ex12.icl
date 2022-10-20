module ex12
import StdEnv

// 1. write a function duplicates which checks if there are neighbour duplicates in a list

duplic :: [Int] -> Bool
duplic [x] = False
duplic [x,y:xs]
| x == y = True
= False || duplic [y:xs]

//Start = duplic [1, 1]
//Start = duplic [2]
//Start = duplic [1, 2, 3, 4, 5, 6, 7, 8, 9]
//Start = duplic [1, 0, 5, 0, 0, 6, 7, 5, 0, 0, 0, 8, 0, 5, 0, 0, 0] 


// 2. write a function that removes neighbour duplicates in a list
duplicrem :: [Int] -> [Int]
duplicrem [x] = [x] 
duplicrem [x,y:xs]
| x == y = duplicrem [y:xs]
= [x] ++ duplicrem [y:xs]

//Start = takeWhile ((<) 5)[2,1,7,4,5]  //dropWhile isOdd [1..5] //[foldr (+) (-1) [-1..2]] //filter (not o isEven) [1,2,4,6,8,10] //takeWhile isOdd [1..5]
//Start = duplicrem [1, 0, 5, 0, 0, 6, 7, 5, 0, 0, 0, 8, 0, 5, 0, 0, 0] 


// 3. transform the sub-sub lists into one list of sublists
f :: [[[Int]]] -> [[Int]]
f list = flatten [ x \\ x <- list ]

//Start = f [[[1,2,3], [3,4,5]], [[1,2,3], [3,4,5], [7,8,9]]] 
// result : [[1,2,3],[3,4,5],[1,2,3],[3,4,5],[7,8,9]]


// 4.  generate the following list [(1,1),(2,2),(3,3),(4,4),(5,5)]
l1 :: [(Int, Int)]
l1 = [ (x,y) \\ x <- [1..10] & y <- [1..10] ]

//Start = l1


// 5. generate [(1,2,3),(2,4,6),(3,6,9),(4,8,12),(5,10,15)]
l2 :: [(Int, Int, Int)]
l2  = [ (x,y,z) \\ x <- [1..5] & y <- [2,4..] & z <- [3,6..] ]

Start = l2