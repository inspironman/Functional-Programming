module ex16

import StdEnv


// 1. Create using an input list of tuples a new list of tuples like:
// [(1,1), (2,6), (3,9)] -> [(1,1,2), (2,6,8), (3,9,12)] 
 

//create :: [(Int, Int)] -> [(Int, Int, Int)]
create list = [ (fst x ,snd x, fst x + snd x) \\ x <- list ]

//Start =  create [(1,1), (2,6), (3,9)]
 

// 2. Compute the average of tuple elements using map

averages :: [(Int, Int)] -> [Int]
averages x = map (\x = (fst x + snd x)/2 ) x

//Start = averages [(1,1), (2,6), (3,9)]


// 3. Put the product of the sublist elements in a list, you must use foldr

sublistsp :: [[Int]] -> [Int]
sublistsp list = [ prod x \\ x <- list ]

//Start = sublistsp [[1, 2, 3], [3, 4], [5, 7, 1]] // [6,12,35]


// 4. Generate the following list of lists
// [[1],[2,1],[3,2,1],[4,3,2,1],[5,4,3,2,1]]

genNlist :: Int -> [[Int]]
genNlist num = [ [x,x-1..1] \\ x <- [1..num] ]
 

//Start = genNlist 5


// 5. "Reverse" a number: 1234 -> 4321

revnrAux :: Int -> [Int]
revnrAux num 
| num > 9 = [num rem 10] ++ revnrAux (num/10)
= [num]

//Start = revnrAux 146

revnr :: Int -> Int
revnr num  
| num < 10 = num
= (num rem 10)*(10^(length (revnrAux num)-1) ) + revnr (num/10)

//Start = revnr 1234


// 6. Delete every second element of a list
// e.g. [1,2,3,4,3,2,4,5] -> [1,3,3,4]
//delsecond :: [Int] -> [Int]

delsecond :: [Int] -> [Int]
delsecond [] = []
delsecond [x] = [x]
delsecond [x,y : xs] = [x : delsecond xs ]


//Start = delsecond [1,2,3,4,3,2,4,5]


// 7. Insert a value after every element of a list 
// [1,2,3,4,5] 0 -> [1,0,2,0,3,0,4,0,5,0]

insertx :: [Int] Int -> [Int]
insertx [x] num = [x,num]
insertx [x:xs] num = [x,num] ++ insertx xs num

//Start = insertx [1,2,3,4,5] 0


// 8. Insert 0 after every digit of a number: 123 -> 102030

 

revnrAux1 :: Int -> [Int]
revnrAux1 num 
| num < 10 = [num]
= revnrAux1 (num/10) ++ [num rem 10]
 

insertx1 :: [Int]  -> [Int]
insertx1 [] = []
insertx1 [x:xs] = [x,0 : insertx1 xs ]

toNum::[Int]->Int
toNum [] = 0
toNum [x:xs] = x*10^((length [x:xs])-1) + toNum xs

digit0 :: Int -> Int
digit0 0 = 0
digit0 num = toNum (insertx1 (revnrAux1 num))

Start = digit0 123 

