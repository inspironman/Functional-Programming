module Deepak_Kumar_Upadhayay_homeWork_1_v_2
import StdEnv

/*
Impelement the zeroFinder function that can detect the presence of the zero digit in a given number
The function returns True if the zero digit exists, and False if it doesn't
The zero can be located at any point in the number
*/



//                                      SOLUTION 1 Q1

digitcounter :: Int -> Int
digitcounter x
| x >= 10 = 1 + digitcounter (x/10)
| otherwise = 1



//Start = digitcounter 146



lastdigit :: Int -> Int
lastdigit x = x rem 10



//Start = lastdigit 146



zeroFinder1 :: Int -> Bool
zeroFinder1 x
| digitcounter x == 1 && lastdigit x == 0 = True
| digitcounter x > 1 && lastdigit x == 0 = True
| digitcounter x > 1 && lastdigit x <> 0 = zeroFinder1 (x / 10)
| digitcounter x < 2 && lastdigit x <> 0 = False

//Start = zeroFinder1 154820 // True
//Start = zeroFinder1 150125 // True
//Start = zeroFinder1 101 // True
//Start = zeroFinder1 7889 // False
//Start = zeroFinder1 0 // True
//Start = zeroFinder1 9 // False
//Start = zeroFinder1 0123// False



//                                     SOLUTION 2 Q1
           
zeroFinder :: Int -> Bool
zeroFinder x 
| x < 10 = x == 0
= (x rem 10) == 0 || zeroFinder (x/10)

//Start = zeroFinder 154820 // True
//Start = zeroFinder 150125 // True
//Start = zeroFinder 101 // True
//Start = zeroFinder 7889 // False
//Start = zeroFinder 0 // True
//Start = zeroFinder 9 // False
//Start = zeroFinder 0123// False


/*
Impelement the caculator function bellow which gets two integer numbers and an operator
in the form of a string and returns the result of
(first number) operator (second number).
Assume that there are only 4 operators which are + , - , / , * .
*/


caculator :: Int Int String -> Int
caculator x y s
| s == "+" = x + y
| s == "-" = x - y
| s == "*" = x * y
| s == "/" = x / y
= abort "Please enter correct Operator"



//Start = caculator 1 2 "+" // 3
//Start = caculator 5 2 "*" // 10
//Start = caculator 8 2 "/" // 4
//Start = caculator 1 1 "-" // 0
//Start = caculator 5 6 "l" //Please enter correct Operator


















