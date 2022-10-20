module Deepak_Kumar_Upadhayay_homeWork_6_v_8
import StdEnv



/* Given a list of Integer and Bool tuples, create a function that 
 * splits the list into two parts
 * where the firt part of the list is all of the elements with True as the
 * second part of the tupe sorted ascendingly based on the value of the integer part
 * and the second part of the list all of the elements with False as the
 * second part of the tupe sorted descendingly based on the value of the integer part
 */

split :: [(Int,Bool)] -> [(Int,Bool)]
split list = zip ( sort [ fst x \\ x <- list | snd x == True ], [snd x \\ x <- list | snd x == True ] ) 
                                      ++ 
           zip ( reverse (sort [ fst x \\ x <- list | snd x == False ]), [snd x \\ x <- list | snd x == False ] ) 


//Start = split [(1,True), (85,True), (1,False), (9,True), (85,False), (45, False)] // [(1,True), (9,True), (85,True), (85,False), (45, False), (1,False)]
//Start = split [(1, True)] // [(1,True)]
//Start = split [] // []

 




/* Write a function that takes list of tuples and checks if it
 * is a valid dictionary. A dictionary item is represented
 * as a (Int, String) tuple, where Int is a key and String is
 * a value and a dictionary is a list of items - [(Int, String)].
 * A tuple list is a valid dictionary if all the keys are unique.
 * Return true if the list is a valid dictionary, false otherwise.
 */ 
 
 
isDict :: [(Int, String)] -> Bool 
isDict list = and [ (fst ((sort list)!!x) <> fst ((sort list)!!y)) \\ x <- [0..((length list) - 2)] & y <- [1..((length list) - 1)] ]


//Start = isDict []                                  // True
//Start = isDict [(1, "x")]                         // True
//Start = isDict [(1, "x"), (4, "y"), (3,"z")]     // True
//Start = isDict [(2, "0"), (2, "1")]             // False
//Start = isDict [(3, "x"), (2, "y"), (3,"z")]   // False
//Start = isDict [(3, "x"), (2, "y"), (4,"z")]  // True

//=========================================================== SOLUTION 2 ===================================================================================

// IF THE QUESTION IS ASKED TO DEAL WITH UNIQUE KEY AND STRING VALUE ALSO THEN PLEASE CONSIDER THIS SOLUTION , OTHERWISE PLEASE IGNORE
               
// In case there is  same Keys and having same string values (duplicate dict) like  [(3, "x"), (2, "y"), (3,"x")], it will return True
// But if the there same key but differnt string value like [(3, "x"), (2, "y"), (3,"z")] then it will return False
// If there is Different Key values for same string values like [(3, "x"), (2, "y"), (4,"x")] then it will return False as well.


isDict2 :: [(Int, String)] -> Bool 
isDict2 list = and [ (((fst ((sort list)!!x) == fst ((sort list)!!y)) && (snd ((sort list)!!x) == snd ((sort list)!!y)))) 
                                                      || 
                   (((fst ((sort list)!!x) <> fst ((sort list)!!y)) && (snd ((sort list)!!x) <> snd ((sort list)!!y))))  
                                                      \\
                                 x <- [0..((length list) - 2)] & y <- [1..((length list) - 1)] ]

//Start = isDict2 []                                  // True
//Start = isDict2 [(1, "x")]                         // True
//Start = isDict2 [(1, "x"), (4, "y"), (3,"z")]     // True
//Start = isDict2 [(2, "0"), (2, "1")]             // False
//Start = isDict2 [(3, "x"), (2, "y"), (3,"z")]   // False
//Start = isDict2 [(3, "x"), (2, "y"), (4,"z")]  // True
//Start = isDict2 [(3, "x"), (2, "y"), (4,"x")] // False
//Start = isDict2 [(3, "x"), (2, "y"), (3,"x")]// True


















