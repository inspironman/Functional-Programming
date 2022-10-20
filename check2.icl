module check2
import StdEnv

/** Insert sum of elements as last element in every sublist of a list */

/*
insertLast ::  [Int] -> [Int]
insertLast list = list ++ [sum list]

addsum :: [[Int]] -> [[Int]]
addsum [] = []
addsum lists = map insertLast lists
*/

addsum :: [[Int]] -> [[Int]]
addsum [] = []
addsum [x:xs] = [x ++ [sum(x)] : addsum xs]


//Start = addsum [[1,2],[2,3,5],[5,6,8,6],[5,6,4,7,1]]

/* Write a function that checks if each elements in the list appears even times or not*/

checkEvenAux :: [Int] Int -> Bool
checkEvenAux list num = isEven ( length (filter ((==) num) list))

checkEven :: [Int] -> Bool
checkEven [] = False
checkEven list = and (map (checkEvenAux list) list)

//Start = checkEven [1,1,2,2,2,2,5,3,5,3]

/* 
Insert x as second element in every sublist of a list. If the sublist is empty then
   x will be the only element int the new sublist.
*/

insertAux:: Int [Int] -> [Int]
insertAux x [] = [x]
insertAux n [x:xs] = [x,n:xs]

insertAtTwo:: Int [[Int]] -> [[Int]]   
insertAtTwo num list = map (insertAux num) list

//Start = insertAtTwo 10 [[1,2],[3,4,5],[],[8]]

/* Compute the average of a list of a float point number using the foldr function */

avg :: [Real] -> Real
avg [] = 0.0
avg [x:xs] = foldr (+) 0.0 [x:xs] / toReal (length [x:xs])

//Start = avg [16.2,17.8,11.5]


// Given a list of lists of Integers, For each sublist keep only the numbers which have at
// most 3 divisors.

divisorAux :: Int Int [Int] -> [Int]
divisorAux inc stop list
| inc > stop = list
| stop rem inc == 0 = [inc] ++ list ++ divisorAux (inc+1) stop list
= divisorAux (inc+1) stop list

divisors :: Int -> [Int]
divisors num = divisorAux 1 num []

//Start = divisors 10

hasMost3 :: Int -> Bool
hasMost3 number = length (divisors number) < 4

//Start = hasMost3 10


maxDiv3 :: [[Int]] -> [[Int]]
maxDiv3 list = map (filter hasMost3) list

//Start = maxDiv3 [[],[2,1,6,5],[4],[8,9,10]]

//Use foldr to check if the square root of each integer in a list are all integer

pfsqAUX :: Int -> Bool
pfsqAUX x = toReal(toInt(sqrt(toReal x))) == sqrt(toReal x)

//Start = pfsqAUX 17

pfsq :: [Int] -> Bool
pfsq list = foldr (\x y = (pfsqAUX x) && y) True list

//Start = pfsq [4,9,10]

sumTwoTuple :: (Int,Int) (Int,Int) -> (Int,Int)
sumTwoTuple (a,b)(c,d) = (a+c,b+d)

//sumTuple :: [(Int,Int)] -> (Int,Int)
//sumTuple [(a,b):xs] = (sum(map fst [(a,b):xs]) ,  sum(map fst [(a,b):xs] ))
//sumTuple [x:xs] = sumTwoTuple x  (sumTuple xs ) 
//sumTuple [] = (0,0)

sumTuple list =  (sum (fst (unzip list)) , sum(snd (unzip list)))



//Start = sumTuple [(1,2),(3,4),(5,3)]

/*
Write a function that takes a list of numbers and 
adds the first element, substract the second element, 
adds the third element , substract the fourth element,
in this alternating repetition.
*/

alternatingSum :: [Int] -> Int
alternatingSum list = sum (map (\(element,index) | isEven index = element = (~ element) ) (zip (list,[0..])))  

//Start = alternatingSum [2,3,4,5,6]

// Give a list of list for each list, extract the first, middle and last element of the list.
// example :- [[1..9],[2..6],[3..11],[1..10]] = [ (1,5,9),(2,4,6),(3,7,11),(1,6,10) ]

point3aux :: [Int] -> (Int,Int,Int)
point3aux list = (hd list, list!!((length list )/2), last list)

//Start = point3aux [1..10]

point3 :: [[Int]] -> [(Int,Int,Int)]
point3 list = map point3aux list 

//Start = point3 [[1..9],[2..6],[3..11],[1..10]]

/*
Write a function that takes a condition and a list of tuples and returns a single tuple
which is the sum of all tuples which satisfy the condition.
Assume the sum of two tuples (a,b) + (c,d) is equal to (a+c , b+d)
*/
 
//selectiveSum :: Bool [(a,b)] -> (a,b)
 
/*
Given a list of non-negative numbers arrange them from smallest to largest so
that each item will be bigger than the previous one exactly by 1.
It may need some additional numbers to be able to accompolish that.
Determine the minimum number of additional needed.
*/

minimumNum :: [Int] -> Int
//minimumNum list = length [(minList list)..(maxList list)] - length list

minimumNum list = length (removeMembers  [(minList list)..(maxList list)] list)

//Start = minimumNum [1,5,4,6]

/*
 Given a list of football teams and a boolean, if the latter is :
 True : Generate matches in which all teams face each other twice (home and away matches)
 False : Generate matches in which all teams face each other once.
 
 USE list comprehensions. If the number of teams is less than 2, return an empty list,
 since there can't be any matches 
 
*/

GenerateMatchesAux :: [Char] [Char] [Char] -> [(Char,Char)]
GenerateMatchesAux [] list2 list3 = []
GenerateMatchesAux [x:xs] [] list3 = GenerateMatchesAux xs list3 list3
GenerateMatchesAux [x:xs] [y:ys] list3
| x<>y = [(x,y)] ++ GenerateMatchesAux [x:xs] ys list3
= GenerateMatchesAux [x:xs] ys list3

Start = GenerateMatchesAux ['A','B','C','D'] ['A','B','C','D'] ['A','B','C','D']


//GenerateMatches :: [Char] Bool -> [(Char,Char)] 









































