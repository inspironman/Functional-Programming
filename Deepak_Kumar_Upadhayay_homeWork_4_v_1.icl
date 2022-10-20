module Deepak_Kumar_Upadhayay_homeWork_4_v_1
import StdEnv


/*Given a list of numbers, create a list of all possible distinct three element tuples
e.g: [1,2,3,4] ->[(1,2,3),(1,2,4),(1,3,4),(1,3,2),(1,4,2),(1,4,3)] 
*/

                          /*  SOLUTION 1 USING RECURSION */
//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tripleMakerAux :: [Int] [Int] [Int] [Int] -> [(Int,Int,Int)]
tripleMakerAux [] list2 list3 list4 = []
tripleMakerAux [x:xs] [] list3 list4 = tripleMakerAux xs list3 list3 list4
tripleMakerAux [x:xs] [y:ys] [] list4 = tripleMakerAux [x:xs] ys list4 list4
tripleMakerAux [x:xs] [y:ys] [z:zs] list4 
| x <> y && x <> z && y <> z = [(x,y,z)] ++  tripleMakerAux [x:xs] [y:ys] zs list4
= tripleMakerAux [x:xs] [y:ys] zs list4

//Start = tripleMakerAux [1,2,3,4] [1,2,3,4] [1,2,3,4] [1,2,3,4] //[(1,2,3),(1,2,4),(1,3,2),(1,3,4),(1,4,2),(1,4,3),(2,1,3),(2,1,4),(2,3,1),(2,3,4),(2,4,1),(2,4,3),(3,1,2),(3,1,4),(3,2,1),(3,2,4),(3,4,1),(3,4,2),(4,1,2),(4,1,3),(4,2,1),(4,2,3),(4,3,1),(4,3,2)]


tripleMaker :: [Int] -> [(Int,Int,Int)]
tripleMaker list = tripleMakerAux list list list list

//Start = tripleMaker [1,2,3,4] // [(1,2,3),(1,2,4),(1,3,2),(1,3,4),(1,4,2),(1,4,3),(2,1,3),(2,1,4),(2,3,1),(2,3,4),(2,4,1),(2,4,3),(3,1,2),(3,1,4),(3,2,1),(3,2,4),(3,4,1),(3,4,2),(4,1,2),(4,1,3),(4,2,1),(4,2,3),(4,3,1),(4,3,2)]
//Start = tripleMaker [1,1,1] // []
//Start = length(tripleMaker [1..100]) // 970200


                    /*   SOLUTION 2 USING LIST COMPREHENSION   */ 
//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------                   
                    

tripleMaker2 :: [Int] -> [(Int,Int,Int)]
tripleMaker2 list = [(a,b,c) \\ (a,b,c)<-[(x,y,z)\\ x<-list,y<-list,z<-list] | a <> b && b <> c && a <> c ] 

//Start = tripleMaker2 [1,2,3,4] //[(1,2,3),(1,2,4),(1,3,2),(1,3,4),(1,4,2),(1,4,3),(2,1,3),(2,1,4),(2,3,1),(2,3,4),(2,4,1),(2,4,3),(3,1,2),(3,1,4),(3,2,1),(3,2,4),(3,4,1),(3,4,2),(4,1,2),(4,1,3),(4,2,1),(4,2,3),(4,3,1),(4,3,2)]
//Start = length(tripleMaker2 [1..100]) // 970200

//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




/*
Given a list of integers and an integer target, return a four element tuple that contains elements 
form the list that sum up to the value of the given target.
if there are no elements that sum up to the target, return an empty list
e.g: [1,2,3,4,142,32] 10 -> [(1,2,3,4)] 
*/

                       /*  SOLUTION 1 USING RECURSION  */
//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fourSum :: [Int] Int -> [(Int,Int,Int,Int)]
fourSum list num
|length list < 4 = []
|sum (take 4 list) == num = [(list!!0,list!!1,list!!2,list!!3)] ++ fourSum (drop 4 list) num
= fourSum (drop 4 list) num

//Start = fourSum [1,2,3,4,142,32] 10 // [(1,2,3,4)]
//Start = fourSum [1,1,1,5,2,2,2,2] 8 // [(2,2,2,2), (1,1,1,5)]
//Start = fourSum [1,1,1,5,2,2,2,2] 250 // []


                       /*   SOLUTION 2 USING LIST COMPREHENSION   */
//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
fourSum1 :: [Int] Int -> [(Int,Int,Int,Int)]
fourSum1 list num = [(list!!a,list!!b,list!!c,list!!d) \\ a<-[0..(length list)-4],b<-[1..(length list)-3],c<-[2..(length list)-2],d<-[3..(length list)-1]  ]

Start = fourSum1 [1,2,3,4,142,32] 10 // [(1,2,3,4)]
//Start = fourSum1 [1,1,1,5,2,2,2,2] 8 // [(2,2,2,2), (1,1,1,5)]
//Start = fourSum1 [1,1,1,5,2,2,2,2] 250 // []

// This Solution is not working properly, TO BE DISCUSS IN CONSULTATION why not working ?


//================================================================================================================================================================================================













