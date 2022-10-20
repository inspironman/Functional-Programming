module prac1
import StdEnv

/*
Given an integer number, return true if the count of digits is even,
or return false if the count of the digits are false.
*/

countDigit1 :: Int -> Int
countDigit1 x
| x < 10 = 1
| x >= 10 = 1 + countDigit1(x/10)

//Start = countDigit1 146

isDigit :: Int -> Bool
isDigit x = (countDigit1 x) rem 2 == 0

Start = isDigit 1466 
 