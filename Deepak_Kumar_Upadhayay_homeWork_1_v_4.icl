module Deepak_Kumar_Upadhayay_homeWork_1_v_4
import StdEnv

 
/*
1-
 Given a number consists of exactly five digits,  write the function 'isOddPalindrome '
 which decides whether the given number is palindrome and odd number or not.
 A number is palindrome if it remains the same when its digits are reversed.

    Example :

If the given number is 12321, the output should be True, 
because when the digits of 12321 are reversed, we get the same number and it's also an  odd number.

*/

digitcount :: Int -> Int
digitcount x 
| x >= 10 = 1 + digitcount (x/10)
= 1

//Start = digitcount 143553 

rev :: Int -> Int
rev x 
|x >= 10 = (x rem 10)*(10^(digitcount x-1)) + rev (x/10)
= x 

//Start =  rev 1456010


isOddPalindrome :: Int -> Bool
isOddPalindrome x = x == rev x && x rem 2 <> 0 && digitcount x == 5


//Start = isOddPalindrome 12321 // True

Start = isOddPalindrome 0 // False Why ???
 
//Start = isOddPalindrome 13222 //  False

//Start = isOddPalindrome 22222 // False

//Start = isOddPalindrome 23232 // False

//Start = isOddPalindrome 75557 // True 









/*

2-
 Complete the function oddDigs which takes an integer number, calculates the sum of its digit, 
 and then finds if that number is odd or not
 If the number is odd, then the function should return True, otherwise it returns False

*/

sum :: Int -> Int
sum x
| x >= 10 = x rem 10 + sum (x/10)
= x

//Start = sum 1456

oddDigs :: Int -> Bool
oddDigs x = (sum x) rem 2 <> 0  


//Start = oddDigs 1234 // False
//Start = oddDigs 55555 // True
//Start = oddDigs 1010 // False






















