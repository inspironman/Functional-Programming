module quiz
import StdEnv

power x n
| n == 0 = 1
| n > 0 = x * power x (n - 1)

//Start = power 5 2

add100 :: Real -> Real
add100 x = x + 100.0

Start = add100 3