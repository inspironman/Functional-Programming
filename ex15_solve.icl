module ex15_solve

import StdEnv


// 1. Compute the sum of the elements of the sublists of a list (you must use map) 
plist :: [[Int]] -> [Int]
plist list = map sum list 

//Start = plist [[1, 2, 3], [3, 4], [5, 7, 8, 9], []]


// 2. Insert x as second element in every sublist of a list.
// if the sublist was empty then x will be the only element in the new sublist. 
// [[1,2], [3,4,5], [6,5,9,7], [], [8]] 10 -> [[1,10,2], [3,10,4,5], [6,10,5,9,7], [10], [8,10]]
insert :: [Int] Int -> [Int]
insert list n 
| length list <> 0 = [hd list : n : tl list]
| otherwise = [n]

xlist :: [[Int]] Int -> [[Int]]
xlist list n = [insert l n \\ l <- list]

//Start = xlist [[1,2], [3,4,5], [6,5,9,7],[], [8]] 10


// 3. Given a list of triple tuples make a tuple of 3 lists like:
f1 :: [(Int, Int, Int)] -> [Int]
f1 list = [fst3 a \\ a <- list]

f2 :: [(Int, Int, Int)] -> [Int]
f2 list = [snd3 a \\  a <- list]

f3 :: [(Int, Int, Int)] -> [Int]
f3 list = [thd3 a \\ a <- list]

clist :: [(Int, Int, Int)] -> ([Int], [Int], [Int])
clist list = (f1 list, f2 list, f3 list)


//Start = clist [(1,2,1), (3,1,4), (8,5,4), (5,7,0), (8,9,1)]  // ([1,3,8,5,8],[2,1,5,7,9],[1,4,4,0,1])


// 4. Generate the first 10 positive elements of a list in which a number is multiple of 25, but is not multiple of 100.
glist :: [Int]
glist = take 10 [n \\ n <- [1..] | n rem 25 == 0 && n rem 100 <> 0]

//Start = glist


// 5. Generate pairs like in the following: 
// [[1,2,3], [4,5], [6,1,8], []] -> [(1,6),(2,20),(3,48),(4,1)]
fpair :: [[Int]] -> [(Int, Int)]
fpair list = [(x, prod y) \\ x <- [1..] & y <- list]

//Start = fpair [[1,2,3],[4,5],[6,1,8],[]]


// 6. Generate the following list
// [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5],[1,2,3,4,5,6],
// [1,2,3,4,5,6,7],[1,2,3,4,5,6,7,8],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9,10]]
nlist :: Int -> [[Int]]
//nlist 0 = []
//nlist n = [[x \\ x <- [1..n]] : nlist (n-1)]

//Start = reverse (nlist 10)

//nlist n = [[x \\ x <- [1..y]] \\ y <- [1..n]]

nlist n = [[1..y]\\ y<-[1..n]]

//Start = nlist 10


// 7. Check if a list contains 3 equal elements one after the other 
// (it can be anywhere in the list) 
// for [1,2,3,3,3,2,4,5] is True for [1 .. 5] is False

dlist :: [Int] -> Bool
dlist [] = False
dlist [x] = False
dlist [x,y] = False
dlist [x,y,z: t] 
| x == y && y == z = True
= dlist [y,z:t] 
 
//Start = dlist [1,2,2,3,4,3,3,2,4,5,5,5] 
//Start = dlist [1..5]

// 8. Extract the third element of the sublists (if there is no such element, ignore that sublist)
// [[1,2,3], [3,4,5,6], [], [5,7,8,11], [1], [8,9]]-> [3,5,8]
qlist :: [[Int]] -> [Int]
qlist list = [ x !! 2 \\ x <- list | length x >= 3]

//Start = qlist [[1,2,3], [3,4,5,6], [], [5,7,8,11], [1], [8,9]]

