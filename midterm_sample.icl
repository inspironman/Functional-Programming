module midterm_sample

import StdEnv

/* 1.
Given a list of Integers, write a function which will keep only the numbers divisble by 10 but not divisible by 4,
and remove the rest.
[10,20..100] = [10,30,50,70,90] beacause 20,40,60,80,100 are divisible by both 10 and 4
*/
removeDiv10No4::[Int]->[Int]
removeDiv10No4 list = [ x \\ x <- list | x rem 10 == 0 && x rem 4 <> 0 ]

//Start = removeDiv10No4 [10,20..100]//[10,30,50,70,90]
//Start = removeDiv10No4 [4,8..30]//[]
//Start = removeDiv10No4 [0,5..50]//[10,30,50]
//Start = removeDiv10No4 []

/* 2.
Write a function that converts binary numbers to decimal numbers.
Each digit of a binary number corresponds to a power of two.
For example: 
10010 = 1 0 0 1 0 = 1*(2^4) + 0*(2^3) + 0*(2^2) + 1*(2^1) + 0*(2^0) = 18
Note the correspondance between 1's and 0's and their multiplication with
the respective powers of two.
*/
DigitCount :: Int -> Int
DigitCount num 
| num < 10 = 1
= 1 + DigitCount (num/10)

toList :: Int -> [Int] 
toList num 
| num < 10 = [num]
= [(num rem 10)] ++  toList (num/10)

//Start = (toList 14560)

binaryToDecimal :: Int -> Int
binaryToDecimal num = sum [ x*(2^y) \\ x <- (reverse (toList num)) & y  <- [((DigitCount num)-1), ((DigitCount num)-2)..] ]

//Start = binaryToDecimal 1 // 1
//Start = binaryToDecimal 0 //0
//Start = binaryToDecimal 10010 //18
//Start = binaryToDecimal 1010101010101 //5461

/* 3.
Given a list of lists, for each list, extract the smallest, medium and largest elements. 
*/

extract :: [[Int]] -> [(Int, Int, Int)]
extract [[]] = [] 
extract list = sort [ (minList x, (minList x + maxList x+1)/2 , maxList x) \\ x <- list ]


//Start = extract [[]] //[]
//Start = extract [[1..9], [2..6], [3..11], [1..10]] // [(1,5,9),(1,6,10),(2,4,6),(3,7,11)]
//Start = extract [[11..19], [2..20], [3..11]] // [(2,11,20),(3,7,11),(11,15,19)]

/* 4.
Write a function that takes a condition, and a list of tuples,
and returns a single tuple which is the sum of all tuples
which satisfy the condition.
Assume the sum of two tuples (a,b) + (c,d) is equal to (a+c, b+d)
*/
selectiveSum :: ( (Int,Int) -> Bool) [(Int,Int)] -> (Int,Int)
selectiveSum cond list =  ( sum [ fst a \\ a <- list] , sum [ snd b \\ b <- list ])

//Start = selectiveSum (\(a,b) -> False) [(1,2),(2,3),(3,4),(4,3),(2,1),(3,2),(1,1),(0,10),(45,-152)] // (0,0)
//Start = selectiveSum (\(a,b) -> a < b) [(1,2),(2,3),(3,4),(4,3),(2,1),(3,2),(1,1),(0,10),(45,-152)] // (6,19)
//Start = selectiveSum (\(a,b) ->  isEven a && isOdd b) [(1,2),(2,3),(3,4),(4,3),(2,1),(3,2),(1,1),(0,10),(45,-152)] // (8,7)
//Start = selectiveSum (\(a,b) -> a > 0 && a < 20 && b > 0 && b < 20)  [(1,2),(2,3),(3,4),(4,3),(2,1),(3,2),(1,1),(0,10),(45,-152)] // (16,16)


/* 5.
Given a list of points, represented as tuple of real numbers, create a list
containing all the unique distances between them. (Here by unique we mean for two points a and b we only have to calculate the distance from A to B
and not from B to A as well)
Do not compute distance of a point to itself.
Distance between two points A and B can be found by the formula : square root of ((A.x - B.x)^2 + (A.y - B.y)^2) . 
Here a point A is represented as a tuple (x,y) where first element is the coordinate x and the second is the coordinate y
*/
//findDistance :: [(Real,Real)] -> [Real]


//Start = findDistance [(1.0,1.0),(4.0,5.0)]//[5]
//Start = findDistance [(1.0,1.0),(4.0,5.0),(1.0, ~6.0),(~1.0,~3.0)]
//[5,7,11.4017542509914,4.47213595499958,9.4339811320566,3.60555127546399]
//Start = findDistance [] // []
//Start = findDistance [(1.0,1.0)] //[]

/*6.
Given a list of lists of Integers and an Integer.
Write a function which will return a list of the maximal elements of each list which are smaller than the given Int.
It is guaranteed that each list has at least one element less than the given Int
NOTE: [[1,3],[1,4]] 2  = [1,1] and not []
*/
maxLessThanN::[[Int]] Int -> [Int]
maxLessThanN list num =  [ maxList y \\ y <- [ filter ((>) num )x \\ x <- list ] ] 
//filter ((>) num )(flatten [ x \\ x <- list ])

//Start =  maxLessThanN [[1,3],[1,4]] 2//[1,1]
//Start = maxLessThanN [[5,2,3,5,2,3,7],[2],[2,3,2,1],[-12,5,7]] 3//[2,2,2,-12]
//Start =  maxLessThanN [[6,2,9],[1,3,1,4],[1..10]] 9//[6,4,8]
//Start =  maxLessThanN [] 4 //[]

/* 7.
Given a list of integers, return a list of tuples:
write a function which goes through the element and for every
element i, return a tuple:
where the first element is the i-th Fibonnaci number,
and the second element is the product of all previous Fibonnaci numbers
Example [0,1,2] -> [(1,1),(1,1),(2,2)]
*/

iFib :: Int -> Int
iFib 0 = 1
iFib 1 = 1
iFib num = iFib (num-1) + iFib (num-2)

//Start = iFib 7

//pFib :: Int -> [Int]
//pFib 0 = 1
//pFib 1 = 1
//pFib num = pFib (iFib num) * pFib (iFib (num-1))

//Start = pFib 7

//generateFib :: [Int] -> [(Int, Int)]



//Start = generateFib [0,1,2] // [(1,1),(1,1),(2,2)]
//Start = generateFib [3,7,11] //[(3,6),(21,65520),(144,1570247078400)]
//Start = generateFib [4..10] //[(5,30),(8,240),(13,3120),(21,65520),(34,2227680),(55,122522400),(89,10904493600)]

/* 8.
Given two integers x and n ,write a function which generates a list of the first n prime numbers
starting from a number x.
Both x and y should be positive.
A prime number is a number which is divisible only by 1 and itself.
1 is not a prime number
*/

isPrime :: Int -> Bool
isPrime num = length [ x \\ x <- [1..num] | num rem x == 0 ] == 2

//Start = isPrime 15

listOfPrimes :: Int Int -> [Int]
listOfPrimes x n = take n [ a \\ a <- [x..] | isPrime a  ]

//Start = listOfPrimes 10 5 // [11,13,17,19,23]
//Start = listOfPrimes 1000 10 // [1009,1013,1019,1021,1031,1033,1039,1049,1051,1061]
//Start = listOfPrimes -10 3 // []
//Start = listOfPrimes 100 -4 // []
//Start = listOfPrimes 100 0 // []


/* 9.
Write a function which generate the Aliquot sequence of number k
Definition:
In mathematics, an aliquot sequence is a sequence of positive integers
in which each term is the sum of the proper divisors of the previous term.
For example, the aliquot sequence of 10 is 10, 8, 7, 1, 0 because:
 divisors of 10 = 5,2,1   their sum = 8
 divisors of 8 = 4,2,1    their sum = 7
 divisors of 7 = 1        their sum = 1
 divisors of 1 = 0        their sum = 0
*/

divsum :: Int -> Int 
divsum num = sum [ x \\ x <- [1..num-1] | num rem x == 0 ]


getSeq :: Int -> [Int]
getSeq 0 = []
getSeq num =  [divsum num] ++ getSeq (divsum num)

//Start = getSeq 10 //[10,8,7,1,0]
//Start = getSeq 11 // [11,1]
//Start = getSeq 9 //[9,4,3,1,0]
//Start = getSeq 100 //[100,117,65,19,1,0]

/* 10.
Write the function which is given the line from the pascal triangle
and calculates next line based on it.
Note: In pascal triangle there are n elements in n-th line. First and
last elements of each line are 1s. Others are calculated based on the
previous line: k-th element is equal to previous line's (k-1)-th and k-th
elements sum. pascal(i,k) = pascal(i-1,k-1) + pascal(i-1,k) for all
elements except first and last.
First 5 levels of pascal triangle:
lvl1: 1
lvl2: 1 1
lvl3: 1 2 1
lvl4: 1 3 3 1
lvl5: 1 4 6 4 1
*/
pascalLine :: [Int] -> [Int]
pascalLine [] = []
pascalLine [x] = [x,x+x]
pascalLine [x,y] = [x,x+y,y]
pascalLine [x,y,z] = [x,x+y,y+z,z]
pascalLine [x,y,z:xs] = [x] ++ [x+y] ++ [y+z] ++  pascalLine xs

//Start = pascalLine [] // [1]
//Start = pascalLine [1] // [1,1]
//Start = pascalLine [1,1] // [1,2,1]
//Start = pascalLine [1,4,6,4,1] // [1,5,10,10,5,1]
//Start = pascalLine [1,6,15,20,15,6,1] // [1,7,21,35,35,21,7,1]

/* 11
Determine the prime factors of a given positive integer.
Construct a list containing the prime factors and their multiplicity in a tuple.
Natural number N > 1 is prime if it is divisible only by 1 and N.
Multiplicity is how many times number can be divided by another one.
Example: multiplicity of 3 in 9 is two, since we can divide 9 by 3 two times.
Another example: multiplicity of 2 in 24 is 3, since 24 can be divided by 2 three times.
*/

factors :: Int -> [Int]
factors num = [ x \\ x <- [2..num] | num rem x == 0 ]

//Start = factors 315

divCount :: Int Int -> Int 
divCount 0 _ = 0
divCount 1 _ = 0 
divCount num fac 
| num rem fac == 0 = 1 + divCount (num/fac) fac 
= 0

//Start = divCount 13 2

primeFactors :: Int -> [(Int, Int)]
primeFactors num = [ (x,divCount num x) \\ x <- factors num | isPrime x ]


//Start = primeFactors 13 // [(13, 1)]
//Start = primeFactors 9 // [(3, 2)]
Start = primeFactors 315 // [(3,2),(5,1),(7,1)]
//Start = primeFactors -315 // [(3,2),(5,1),(7,1)]
//Start = primeFactors 0 // []
//Start = primeFactors 1 // []

/* 12.
Generate the combinations of K distinct objects chosen from the N elements of a list
In how many ways can a committee of 3 be chosen from a group of 12 people?
We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients).
We want to really generate all the possibilities in a list.
*/
//combinations :: Int [a] -> [[a]]
//Start = combinations 5 [1,2,3,4,5] // [[1,2,3,4,5]]
//Start = combinations 5 [1,2,3] // []
//Start = combinations 2 ["a", "b", "c", "d"] // [["a","b"],["a","c"],["a","d"],["b","c"],["b","d"],["c","d"]]
//Start = length (combinations 3 ['a'..'z']) // 2600
//Start = combinations 2 [True, False, True] // [[True,False],[True,True],[False,True]]
