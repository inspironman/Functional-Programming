module firstlab
import StdEnv


/*Write a function that takes two arguments, say n and x, and computes their power.*/

power :: Int Int-> Int
power x n
| n == 0 = 1
| n == 1 = x
= x * power x (n-1)

//Start = power 5 4

//construct a function that squares its argument

fact :: Int -> Int
fact x 
| x < 0 = 1
| x == 0 = 1
| x == 1 = 1
= x * fact(x-1)

//Start = fact 5

//We get three number as parameter decide if any of them is sum of two others 

sumofThree :: Int Int Int -> Bool
sumofThree x y z 
| x + y == z = True
| y + z == x = True
| x + z == y = True
| otherwise = False

//Start = sumofThree 3 2 11


// Given two integers put their integers together like 123 652 = 123652 .

countdigits :: Int -> Int 
countdigits x 
| x >= 10 = 1 + ( countdigits (x/10) )
= 1

//Start = countdigits 32

puttogether :: Int Int -> Int 
puttogether x y = x * (10^(countdigits y)) + y

//Start = puttogether 146 653

putstringtogether :: Int Int -> Int
putstringtogether x y = toInt (toString (x) +++ toString (y)) 

//Start = putstringtogether 146 653

 
/*
Write a function that will calculate the total of your dining bill after including gratuity.
Take the subtotal as a Real and the gratuity percentage as a Real.

For example:
If the subtotal is 10.00, and gratuity is 0.15, then
the total is 10.00 + (0.15 * 10.00) = 11.50
*/

myBill :: Real Real -> Real 
myBill x y = x + (y*x)

//Start = myBill 10.00 0.15

//Start = myBill 9001.00 0.08 //9721.08

add100 :: Real -> Real
add100 x = x + 100.0

Start = add100 3






























: