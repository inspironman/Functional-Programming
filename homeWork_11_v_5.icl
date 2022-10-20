module homeWork_11_v_5
import StdEnv


/* Write a function that gets a text (string)
 * an integer key and returns encrypted text back.
 * Encryption should be done with Caesar Cipher. 
 * Encryption with Caesar code is based on an alphabet shift.
 * For example if key is 3, every character is changed with one
 * 3 steps right from it:
 * Plain Alphabet         - ABCDEFGHIJKLMNOPQRSTUVWXYZ
 * Caesar Alphabet, Key=3 - DEFGHIJKLMNOPQRSTUVWXYZABC
 *
 * So, if the text is "apple" and key is 3, encrypted text
 * would be 'dssoh'.
 * 'a' +3 -> 'd'
 * 'p' +3 -> 's'
 * 'l' +3 -> 'o'
 * 'e' +3 -> 'h'
 *
 * You can assume that text contains only lower case letters
 * but the key can be any integer. It can be greater than 26
 * or negative. Negative keys rotate alphabet to the left.
 */
 
aux1 :: [Char] Int -> [Int]
aux1 [] a = []
aux1 [x:xs] a
| ( (( ((toInt x) + a) > 96 && ((toInt x) + a) < 123 ) || ( ((toInt x) + a) > 64 && ((toInt x) + a) < 91)) )  = [((toInt x) + a)] ++ aux1 xs a
= [((toInt x) - 26 + a)] ++ aux1 xs a

aux2 :: [Char] Int -> [Char]
aux2 [] a = []
aux2 list a = [ toChar x \\ x <- (aux1 list a) ]

aux3 :: String -> [Char]
aux3 str = [ x \\ x <-: str]


caesar :: String Int -> String
caesar str a 
| a < 0 = caesar str (26 + a)
| a > 26 = caesar str (a - 26)   
= { p \\ p <- list }          
where 
     list = (aux2 (aux3 str) a)
          
//Start = caesar "apple" 3 // "dssoh"
//Start = caesar "apple" -5 // "vkkgz"
//Start = caesar "apple" 30 // "ettpi"
//Start = caesar "apple" 26 // "apple"
//Start = caesar "caesar" 7 // "jhlzhy"

//Start = caesar "abcdefghijklmnopqrstuvwxyz" 3
//Start = caesar "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 3






/* Given 2 string find if the 2nd one can be substring
 * of the 1st one. A string is a substring of some other
 * string if it can be obtaine by removing some characters.
 * For example, string 'abcb' has following substrings:
 * a, b, c, ab, ac, bc, bb, cb, abc, abb, acb, bcb.
 * Characters can not be rearranged! Only removing character is
 * allowed.
 */

 
index :: [Char] Char -> Int
index [] c = 0
index [x:xs] c 
| x == c = 0
= 1 + index xs c

//Start = index ['a','p','p','l','e']  'p'
 
allIndex :: [Char] [Char] -> [Int]
allIndex a b = [ index a x \\ x <- b ]

//Start = allIndex ['a','p','p','l','e']  ['p','p']

isSub2 a b = [ x \\ x <-: b | isMember x (aux3 a) ]

//Start = aux3 "z"

//Start = isSub2 "a" "z"
//Start = isSub2 "string" "sub"

isSub :: String String -> Bool
isSub a b 
| length f1 < 1 = allIndex (aux3 a) (isSub2 a b) == allIndex (aux3 b) (aux3 b)
| length f2 < 1 = allIndex (aux3 a) (isSub2 a b) == allIndex (aux3 b) (aux3 b)
= and [ (x > y || x == y) && (f3 == f4) \\ x <- f1 & y <- f2 ]
where 
     f1 = allIndex (aux3 a) (isSub2 a b)
     f2 = allIndex (aux3 b) (aux3 b)
     f3 = (aux3 b)  
     f4 = (isSub2 a b)
 
//Start = isSub "abcb" "bb" // True
//Start = isSub "abcb" "ab" // True
//Start = isSub "abcb" "ba" // False
//Start = isSub "string" "sub" // False
//Start = isSub "string" "sing" // True
//Start = isSub "a" "z" // False
//Start = isSub " " "   " // True

//Start = aux3 "z" == isSub2 "a" "z"
//Start = allIndex (aux3 "a") (isSub2 "a" "b")
//Start = allIndex (aux3 "z") (isSub2 "a" "z") == allIndex (aux3 "z") (isSub2 "z" "z")





















