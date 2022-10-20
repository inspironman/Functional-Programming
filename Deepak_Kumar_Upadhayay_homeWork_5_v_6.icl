module Deepak_Kumar_Upadhayay_homeWork_5_v_6
import StdEnv


/* Write a function that takes a list of integer pairs and sorts
 * it in ascending order. Pairs are given as a 2 element tuple.
 * Use first element to compare tuples, if first elements are equal
 * than use second element for comparison.
 * For example:
 * (1, 2) < (2, 3)
 * (1, 2) < (2, 0)
 * (1, 2) < (1, 3)
 * (1, 2) = (1, 2)
 * (1, 2) > (1, 1)
 * (1, 2) > (0, 3)
 * (1, 2) > (0, 0)
 */

pairSort :: [(Int, Int)] -> [(Int, Int)]
pairSort list = sort list

//Start = pairSort [(1, 2), (2, 3), (2, 0), (1, 1), (0, 0), (1, 2), (1, 5)] // [(0,0),(1,1),(1,2),(1,2),(1,5),(2,0),(2,3)]
//Start = pairSort [] // []
//Start = pairSort [(0, 0)] // [(0, 0)]
//Start = pairSort [(1,3),(1,2)]







/* Create the function matSort that takes a list of list of
 * Integers representing a matrix, and sorts the matrix by columns
 * e.g: 
 * [[17,43,34],[12,442,52],[1,3,2]] represents the following matrix:
 *
 * 17  43  34        1    3    2
 * 12  442  52   ->  12   43   34
 * 1   3    2        17   442  52
 */

//============================= SOLUTION 1 ================================================= 

mat2Sort :: [[Int]] -> [[Int]]
mat2Sort list = sort list

//Start = matSort [] // []
//Start = mat2Sort [[17,43,52],[12,442,34],[1,3,2]] //  [[1,3,2],[12,43,34],[17,442,52]]
//Start = matSort [[1..10], [10..5..10],[20..30]] // [[1..10], [10,9..1],[20..30]] 


//THIS SOLUTION PASSED ALL GIVEN TEST CASES.

//I REQUEST YOU TO PLEASE ADD SOME MORE TEST CASES SO THAT WE CAN HAVE CLEAR IDEA ABOUT THE QUESTION .





//============================== SOLUTION 2 =================================================


sortAux :: [[Int]] -> [[Int]]
sortAux lists = [ [sub !! x \\ sub <- lists] \\ x <- [0..length (hd lists)-1] ]

//Start = sortAux [[17,43,34],[12,442,52],[1,3,2]] 
//[[17,12,1],[43,442,3],[34,52,2]]

sortAux2 list = map sort (sortAux list)

//Start = sortAux2 [[17,43,34],[12,442,52],[1,3,2]] 
 
 
matSort :: [[Int]] -> [[Int]]
matSort [] = []
matSort list =  [[l !! x \\ l <- (sortAux2 list)] \\ x <- [0..length (hd (sortAux2 list))-1]]

//Start = matSort [] // []

//Start = matSort [[17,442,52],[12,43,34],[1,3,2]] //   [[1,3,2],[12,43,34],[17,442,52]]

//Start = matSort [[17,43,34],[12,442,52],[1,3,2]] // [[1,3,2], [12,43,34],[17,442,52]]

//Start = matSort [[1..10], [10,9..1],[20..30]] // [[1..5..1], [10..5..10],[20..25..30]]


















