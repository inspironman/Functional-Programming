module Deepak_Kumar_Upadhayay_homeWork_3_v_2
import StdEnv


/*
1-
    * Given a list of charachters, implement the pack function which does the following:
    * Pack the consecutive duplicates of the  list's elements into sublists,meaning that
    * if the list contains repeated elements they should be placed in separate sublists.
*/
pack :: [Char] -> [[Char]]
pack [] = []
pack [a] = [[a]]
pack [x,y:xs]
|(x==y) = [[x,y] ++ (takeWhile ((==)x) xs)] ++ pack (dropWhile ((==)x) xs)
= [[x]] ++ pack [y:xs]

//Start = pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] // [['a','a','a','a'],['b'],['c','c'],['a','a'],['d'],['e','e','e','e']]
//Start = pack ['a','b','c','d'] // [['a'],['b'],['c'],['d']]
//Start = pack ['a','a','b','b','b','b','b','b','b','b','b','b','b','a','a'] //[['a','a'],['b','b','b','b','b','b','b','b','b','b','b'],['a','a']]
//Start = pack [] // [[]] i think there shoud be an empty list only []
                            

/* 
 * Write a function that takes a list and 2 numbers (K and M) as
 * arguments. Function should find reminder by K of all numbers
 * from the initial list that are greater than M and return their sum.
 * 
*/
 
remSumaux :: [Int] Int -> [Int]
remSumaux [] num = []
remSumaux [x:xs] num = [y] ++ remSumaux xs num
where y = x rem num

//Start = remSumaux [4,8..80] 6
 
remSum :: [Int] Int Int -> Int
remSum list x y = sum (remSumaux (filter ((<) y ) list) x)  

//Start = remSum [1,2,3,4] 2 2 // 1
//Start = remSum [1..1000] 63 541 // 14508
//Start = remSum [1,5,6,1,2] 3 10 // 0
//Start = remSum [2,4,5,7] 3 1 // 6




