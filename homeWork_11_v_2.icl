module homeWork_11_v_2
import StdEnv




/*1-Closest avg
    * Given a list of lists of real numbers, for every sublist find the item in the sublist which is closest 
    * to the average of the sublist.
    * e.g [[1.3, 5.2, 7.7, -2.3, 23.45] , [3.0,8.4] ] ->  avg of [1.3, 5.2, 7.7, -2.3, 23.45]  is 7.07 
    * so the closest value from the list is  7.7
    * similarly  avg of [3.0,8.4] is 5.7  so the closest value from the list is 3.0
*/

avg :: [Real] -> Real
avg list = (sum list / toReal (length list))

avgDiff :: [Real] -> [Real]
avgDiff list = [ x-a \\ x <- list ]
where 
     a = avg list
     
//Start = avgDiff [1.3, 5.2, 7.7, -2.3, 23.45]

minDiff :: [Real] -> Int
minDiff list = hd [ i \\ y <- (avgDiff list) & i <- [0..] |  == minList(avgDiff list) ]

Start = minDiff [1.3, 5.2, 7.7, -2.3, 23.45]

//closestToAvg :: [[Real]] -> [Real]


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

// isSub :: String String -> Bool

// Start = isSub "abcb" "bb" // True
// Start = isSub "abcb" "ab" // True
// Start = isSub "abcb" "ba" // False
// Start = isSub "string" "sub" // False
// Start = isSub "string" "sing" // True
// Start = isSub "a" "z" // False

