module Deepak_Kumar_Upadhayay_pt3_m
import StdEnv

/*
	Change the order of the list. First half should go after the second half.
	
	[1,2,3,4] 	-> [3,4,1,2]
	[1,3,5,7,9] -> [7,9,1,3,5]
*/


changeOrder :: [Int] -> [Int]
changeOrder list 
| length list rem 2 == 0 = drop ((length list)/2) list ++ take ((length list)/2) list
= drop ((length list)/2 + 1) list ++ take ((length list)/2 + 1) list

//Start = changeOrder [1,2,3,4]
//Start = changeOrder [1,3,5,7,9]


