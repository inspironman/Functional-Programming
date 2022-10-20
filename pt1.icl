module pt1
import StdEnv

/*
Given an integer number, return True if the before last digit of the number is even, 
or return False if is Odd.
The number should be bigger then 10.
*/

lastdigit :: Int -> Int
lastdigit x = x / 10

//Start = lastdigit 146


isDigEven :: Int -> Bool
isDigEven x 
| x < 10 = False
| x > 10 = (lastdigit x) rem 2 == 0 
 


//Start = isDigEven 56 // False
//Start = isDigEven 321 // True
//Start = isDigEven  11012255 // False
//Start = isDigEven 0 //"small number"