module et2 
import StdEnv

/*
1.Given two integer numbers k and n.
Generate a String by the following pattern:
In case k=2 and n=3
"[[**[[**[[**"
So n times, k opening brackets followed by k stars.
*/


bracketsStars::Int Int -> String
bracketsStars a b = { x \\ x <- y }
where 
     y = flatten [ [ '[' \\ m <- [ 1..a ] ] ++ [ '*' \\ m <- [ 1..a ] ] \\ o <- [1..b] ]
    
//Start = bracketsStars 2 3 // "[[**[[**[[**"
//Start = bracketsStars 0 3 // ""
//Start = bracketsStars 5 2 // "[[[[[*****[[[[[*****"

/*
2.Write a function which takes a String and modifies it such that
only the first character is uppercase and every other one is lowercase.
The given string contains only letters!

Hints:
toInt('a') - toInt('A') == 32
Uppercase letters are in the range [65,90] (ASCII)
Lowercase letters are in the range [97,122] (ASCII)

Example:
"abcdFeH" = "Abcdfeh"
*/

toList :: String -> [Char]
toList list = [ x \\ x <-: list ]

toStr :: [Char] -> String
toStr [] = ""
toStr [x:xs] = toString x +++ toStr xs

help :: [Char] -> String 
help [] = ""
help [x:xs] 
|toInt x >= 65 && toInt x <= 90 =  toString (toChar (toInt x + 32 )) +++  help xs  
= toString x +++ help xs

capitalFirst :: [Char] -> String 
capitalFirst [] = ""
capitalFirst [x:xs] = toString (toChar (toInt x - 32 )) +++ toStr xs

//Start = capitalFirst ['h','e','l','l','o']

capitalize::String -> String
capitalize word = capitalFirst (toList (help (toList word)))

//Start = capitalize "aABCDEFG" // "Aabcdefg"
//Start = capitalize "abABab" // "Ababab"
//Start = capitalize "" // ""


:: Major = Finance | CS | Math | Physics | Economy | Linguistics
:: Course = {name::String, major::Major, credits::Int}

OOP::Course
OOP = {name="OOP",major=CS, credits=5}
Discrete_math::Course
Discrete_math = {name="Discrete_math",major=Math, credits=4}
Relativity::Course
Relativity = {name="Relativity", major=Physics, credits=6}
Functional::Course
Functional = {name="Functional", major=CS, credits=5}
Quantum_mechanics::Course
Quantum_mechanics = {name="Quantum_mechanics", major=Physics, credits=4}
Corporate_finance::Course
Corporate_finance = {name="Corporate_finance", major=Finance, credits=6}
Venture_captical::Course
Venture_captical = {name="Venture_captical", major=Finance, credits=6}
Macroeconomics::Course
Macroeconomics = {name="Macroeconomics", major=Economy, credits=6}
Microeconomics::Course
Microeconomics = {name="Microeconomics", major=Economy, credits=6}
Numerical_Methods::Course
Numerical_Methods = {name="Numerical_Methods", major=Math, credits=4}
Cryptography::Course
Cryptography = {name="Cryptography", major=CS, credits=2}
Phonology::Course
Phonology = {name="Phonology", major=Linguistics, credits=3}
Morphology::Course
Morphology = {name="Morphology", major=Linguistics, credits=3}

/*
3.Given a list of Courses and a major, check if any of the courses in the list
has the same major with the given one, return True if there is at least one.
*/
instance == Major
where 
     (==) Finance Finance = True
     (==) CS CS = True
     (==) Math Math = True 
     (==) Physics Physics = True
     (==) Economy Economy = True 
     (==) Linguistics Linguistics = True
     (==) _ _ = False 

same_major :: [Course] Major -> Bool
same_major list m = or [ x.major == m \\ x <- list ]

//Start = same_major [Corporate_finance, OOP, Microeconomics] Finance // True
//Start = same_major [Morphology, Macroeconomics, Quantum_mechanics] CS // False
//Start = same_major [Venture_captical, Relativity, Cryptography] Physics // True
//Start = same_major [Discrete_math] Economy // False
//Start = same_major [] Physics // False

/*
4.Given a list of Courses that a student has taken, write a function that
returns a list of records (you need to define by yourself, called Earned_credits),
each records has the name of the major (field) and
the credits the student earned for each major (earned).
*/
//:: Earned_credits = { major1::Major , credit1 :: Int }

 
  
//calc :: [Course] -> [Earned_credits]
 
//Start = calc [Functional, OOP, Relativity] // [(Earned_credits CS 10),(Earned_credits Physics 6)]
//Start = calc [Morphology, Macroeconomics, Numerical_Methods] // [(Earned_credits Math 4),(Earned_credits Economy 6),(Earned_credits Linguistics 3)]
//Start = calc [Corporate_finance, Numerical_Methods, Cryptography] // [(Earned_credits Finance 6),(Earned_credits Math 4),(Earned_credits CS 2)]
//Start = calc [] // []

/*
5.Write '+' operator for lists.
If both lists are sorted in increasing order you should merge them
in a way that resulting list is sorted too.
Ex.: [1,3,6] + [2,4,5,7] -> [1,2,3,4,5,6,7]
If list is not sorted than it is considered empty.
Ex.: [1,3,6] + [2,3,1] -> [1,3,6] + [] -> [1,3,6]
Ex.: [2,9,7] + [5,4,3] -> [] + [] -> []
*/

isSorted :: [a] -> [a] | Eq , Ord a
isSorted list 
| sort list == list = list
= []

do :: [a] [a] -> [a] | Eq , Ord a 
do m n = sort (( isSorted m ) ++ (isSorted n))

//Start = do [1,2,4,5] [3,4,5,66]

//Start = isSorted [1,3,4,5,2]

instance + [a] | Eq ,Ord a
where 
     (+) m n = do m n
//Start = [1,2,3] + [1,3,6] // [1,1,2,3,3,6]
//Start = [1,3,6] + [2,4,5,7] // [1,2,3,4,5,6,7]
//Start = [1,3,6] + [2,3,1] // [1,3,6]
//Start = [5,1] + [1,3,6] // [1,3,6]
//Start = [] + [1] // [1]
//Start = [2,3,1] + [12,3,1] // []
//Start = [] + [] // []

:: Tree a = Node a (Tree a) (Tree a) | Leaf

tree1 :: Tree Int
tree1 = (Node 4 (Node 10 (Node 6 Leaf Leaf)(Node 11 Leaf Leaf)) (Node 20 (Node 12 Leaf Leaf) Leaf))
tree2 :: Tree Int
tree2 = (Node 5 (Node 10 (Node 31 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 17 (Node 31 (Node 14 (Node 12 Leaf Leaf) Leaf) Leaf) (Node 11 Leaf Leaf) ))
tree3 :: Tree Int
tree3 = (Node 12 (Node 11 (Node 11 (Node 32 Leaf Leaf) Leaf) Leaf) (Node 4 (Node 17 (Node 5 (Node 7 Leaf Leaf) Leaf) Leaf) (Node 3 Leaf (Node 4 Leaf Leaf)) ))
tree4 :: Tree Int
tree4 = (Node 7 (Node 11 tree1 tree2) (Node 5 tree3 tree2))
tree5 :: Tree Int
tree5 = Node 1 tree3 tree4

/*
6.Define == for Tree. Two Trees are equal if each node value
from 1st tree is equal to 2nd tree.
*/

lister :: ( Tree a ) -> [a]
lister Leaf = []
lister (Node x l y) = [x] ++ lister (l) ++ lister (y)

//Start = lister tree1

instance == (Tree a) | == a
where 
	(==) t1 t2 = (lister  t1) == (lister t2) 
  

//Start = tree1 == tree1 // True
//Start = tree2 == tree3 // False
//Start = tree4 == tree4 // True
//Start = tree1 == tree5 // False

:: Student = {s_name::String, grades::[Int]}

A :: Student
A = {s_name="A", grades = [1,2,3]}
B :: Student
B = {s_name="B", grades = [1,2,3,4]}
C :: Student
C = {s_name="C", grades = [1,2,3,4,5]}
D :: Student
D = {s_name="D", grades = [1,2,3,4,5,6]}
E :: Student
E = {s_name="E", grades = [1]}
F :: Student
F = {s_name="F", grades = [1,2]}

/*
7.Having a list of students, each student has a list of grades
create a balanced (or almost balanced) binary search tree based on the students' grades average.
Info: balance tree means that the difference between the depth of the left tree and the depth of 
the right tree is less than 1
Note: There is no dublication in the averages.
*/




//createStudTree :: [Student] -> (Tree Student)

 
//Start = createStudTree [] // Leaf
//Start = createStudTree [A,B,C] //(Node (Student "B" [1,2,3,4]) (Node (Student "A" [1,2,3]) Leaf Leaf) (Node (Student "C" [1,2,3,4,5]) Leaf Leaf))
//Start = createStudTree [A,B,C,D] //(Node (Student "C" [1,2,3,4,5]) (Node (Student "B" [1,2,3,4]) (Node (Student "A" [1,2,3]) Leaf Leaf) Leaf) (Node (Student "D" [1,2,3,4,5,6]) Leaf Leaf))
//Start = createStudTree [C,D,A,B] //(Node (Student "C" [1,2,3,4,5]) (Node (Student "B" [1,2,3,4]) (Node (Student "A" [1,2,3]) Leaf Leaf) Leaf) (Node (Student "D" [1,2,3,4,5,6]) Leaf Leaf))
//Start = createStudTree [F,E,C,D,A,B] //(Node (Student "B" [1,2,3,4]) (Node (Student "F" [1,2]) (Node (Student "E" [1]) Leaf Leaf) (Node (Student "A" [1,2,3]) Leaf Leaf))
// (Node (Student "D" [1,2,3,4,5,6]) (Node (Student "C" [1,2,3,4,5]) Leaf Leaf) Leaf))

/*
8.Given binary search tree and Integer value, remove all the nodes from the tree which have this value.
Resulting tree should maintain binary search tree property.
*/

removeInt :: Int (Tree Int) -> (Tree Int)
removeInt num Leaf = Leaf
removeInt num (Node x l r)
| num == x = Leaf 
= (Node x (removeInt num l) (removeInt num r)) 



//Start = removeInt 5 (Node 4 (Node 3 (Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 4 Leaf Leaf)) (Node 5 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf))) // (Node 4 (Node 3 (Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 4 Leaf Leaf)) Leaf)
//Start = removeInt 1 (Node 1 (Node 1 (Node 1 (Node 1 (Node 1 (Node 1 Leaf Leaf) Leaf) Leaf) Leaf) Leaf) Leaf) // Leaf

/*
9.Complementary colors are pairs of colors which, when combined or mixed,
cancel each other out (lose hue) by producing a grayscale color like white or black.
Such pairs are:
Red - Green
Orange - Blue
Yellow - Purple
Violet - Amber
Teal - Vermilion
Magenta - Chartreuse
Create an instance == for the Color and
write a function that finds complement of a given color.
*/

:: Color = Red | Yellow | Green | Blue | Purple | Orange | Violet | Amber | Teal | Vermilion | Magenta | Chartreuse

colorList = [Red, Yellow, Green, Blue, Purple, Orange, Violet, Amber, Teal, Vermilion, Magenta, Chartreuse]

instance == Color 
where 
     (==) Red Green = True
     (==) Green Red = True
     (==) Orange Blue = True
     (==) Blue Orange = True
     (==) Yellow Purple = True 
     (==) Purple Yellow = True
     (==) Teal Vermilion = True
     (==) Vermilion Teal = True
     (==) Magenta Chartreuse = True
     (==) Chartreuse Magenta = True 
     (==) _ _ = False 
	
find_complement :: Color -> Color
find_complement c = hd [ x \\ x  <- colorList | x == c ]
 
//Start = find_complement Red // Green
//Start = find_complement Green // Red
//Start = find_complement Teal // Vermilion
//Start = find_complement Chartreuse // Magenta
//Start = find_complement Violet // Amber






















 