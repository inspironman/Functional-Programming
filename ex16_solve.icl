module ex16_solve

import StdEnv


// 1. Create using an input list of tuples a new list of tuples like:
// [(1,1), (2,6), (3,9)] -> [(1,1,2), (2,6,8), (3,9,12)] 

create :: [(Int, Int)] -> [(Int, Int, Int)]
//create [] = []
//create [(x, y):ts]=[(x,y,x+y):create ts]

create x =  map (\ x = (fst x, snd x, fst x + snd x )) x 

//Start = create [(1,1), (2,6), (3,9)]
 

// 2. Compute the average of tuple elements using map
averages :: [(Int, Int)] -> [Int]
averages x = map (\ x = (fst x + snd x )/2) x 

//Start = averages [(1,1), (2,6), (3,9)]


// 3. Put the product of the sublist elements in a list, you must use foldr
sublistsp :: [[Int]] -> [Int]
sublistsp [] = []
sublistsp [x:xs] = [foldr (*) 1 x \\ x <- [x:xs]]

//Start = sublistsp [[1, 2, 3], [3, 4], [5, 7, 1]] // [6,12,35]


// 4. Generate the following list of lists
// [[1],[2,1],[3,2,1],[4,3,2,1],[5,4,3,2,1]]
genNlistAux:: Int -> [[Int]]
genNlistAux 0 = []
genNlistAux n = [reverse (take n [1..n]) : genNlistAux (n-1)]

//Start = genNlistAux 5

genNlist:: Int -> [[Int]]
genNlist 0 = []
genNlist n = reverse (genNlistAux n)

//Start = genNlist 5


// 5. "Reverse" a number: 1234 -> 4321
//revnr :: Int -> Int
toList::Int ->[Int]
toList n
|n < 10 = [n]
=toList (n/10) ++ [n rem 10]

revnr :: Int -> Int
revnr n
|n < 10 = n
= (n rem 10)*(10^((length(toList n))-1))+revnr (n/10)

//Start = revnr 1234


// 6. Delete every second element of a list
// e.g. [1,2,3,4,3,2,4,5] -> [1,3,3,4]
delsecond :: [Int] -> [Int]
delsecond [] = []
delsecond [x] = [x]
delsecond [x,y : xs] = [x : delsecond xs ]

//Start = delsecond [1,2,3,4,3,2,4,5]


// 7. Insert a value after every element of a list 
// [1,2,3,4,5] 0 -> [1,0,2,0,3,0,4,0,5,0]
insertx :: [Int] Int -> [Int]
insertx [] n = []
insertx [x:xs] n = [x,n : insertx xs n]

//Start = insertx [1,2,3,4,5] 0


// 8. Insert 0 after every digit of a number: 123 -> 102030
//digit0 :: Int -> Int
toListAux::Int ->[Int]
toListAux n
|n < 10 = [n]
= toListAux (n/10) ++ [n rem 10]

insertxAux :: [Int]  -> [Int]
insertxAux [] = []
insertxAux [x:xs] = [x,0 : insertxAux xs ]

toNum::[Int]->Int
toNum [] = 0
toNum [x:xs] = x*10^((length [x:xs])-1) + toNum xs

digit0 :: Int -> Int
digit0 0 = 0
digit0 n = toNum (insertxAux (toListAux n))

Start = digit0 123