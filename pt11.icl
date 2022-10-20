module pt11
import StdEnv




/*1-Closest avg
    * Given a list of lists of real numbers, for every sublist find the item in the sublist which is closest
    * to the average of the sublist.
    * e.g [[1.3, 5.2, 7.7, -2.3, 23.45] , [3.0,8.4] ] ->  avg of [1.3, 5.2, 7.7, -2.3, 23.45]  is 7.07
    * so the closest value from the list is  7.7
    * similarly  avg of [3.0,8.4] is 5.7  so the closest value from the list is 3.0
*/
average :: [Real] -> Real
average lst = (sum lst) / (toReal(length lst))
closest ::  [Real] Real -> [Real]
closest [] avg = []
closest  [x:xs] avg = [(abs(avg - x))] ++ closest xs avg 
locator a [] = 0
locator a [x:xs] 
| a == x = 0
=1+locator a xs
extractor :: Int [a] -> a
extractor n list = list !! n
f1 :: [Real] -> Real
f1 list =  extractor (locator (minList(closest list (average list))) (closest list (average list))) list
closestToAvg :: [[Real]] -> [Real]
closestToAvg list = map f1 list
//Start = closestToAvg [[1.3, 5.2, 7.7, -2.3, 23.45] , [3.0,8.4] ] //  [7.7,3]
//Start = closestToAvg [[2.4 ,4.5 ,6.7 ,6.6 ,7.7] , [5.6 , 6.8 ,4.8 , 4.1] , [5.5,5.1] , [5.0] , [7.8] ] // [6.6,5.6,5.5,5,7.8]
//Start = closestToAvg [[1.3]] // [1.3]



/* Given 2 string find if the 2nd one can be substring
 * of the 1st one. A string is a substring of some other
 * string if it can be obtaine by removing some characters.
 * For example, string 'abcb' has following substrings:
 * a, b, c, ab, ac, bc, bb, cb, abc, abb, acb, bcb.
 * Characters can not be rearranged! Only removing character is
 * allowed.
 */
StringToList str = [x\\x <-: str]


position :: [Char] [Char] -> [Bool]
position [] list = [True]
position [x,y:xs] list 
| (locator x list) > (locator y list) = [False]
= [True] ++ position xs list

member :: [Char][Char] -> [Bool]
member [] list = [True]
member [x:xs] list
| isMember x list = [True] ++ member xs list
= [False] ++ member xs list

isSub :: String String -> Bool
isSub s1 s2 = and (member  (StringToList s2) (StringToList s1) ++ position  (StringToList s2) (StringToList s1))

//Start = isSub "abcb" "bb" // True
//Start = isSub "abcb" "ab" // True
//Start = isSub "abcb" "ba" // False
//Start = isSub "string" "sub" // False
//Start = isSub "string" "sing" // True
//Start = isSub "a" "z" // False