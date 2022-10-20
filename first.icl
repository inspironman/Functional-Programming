module first
import StdEnv

x :: {Int}
x = {1,2,3,4}

//Start = x.[2]

stringToList :: String -> [Char]
stringToList str = [ a \\ a <-: str ]

//Start = stringToList "abcd"

listToString :: [Char] -> String
listToString list = { a \\ a <- list }

//Start = listToString ['a','b','c','d']

//:: Point = { x :: Int , y :: Int }
//:: Cicle = { Radius :: Int , CenterPoint :: Point }

/*
Given three vectors in 2D, decide if their endpoint lie on a same line. 
Three points are on the same line if area of trianngle formed by them is 0;
(x1y2 + x2y3 + x3y1 - x1y3 - x2y1 - x3y2)/2 = 0
*/

:: Vector = { x :: Real , y :: Real }

collinear :: Vector Vector Vector -> Bool
collinear { x = x1, y = y1} { x = x2, y = y2} { x = x3, y = y3} = (x1*y2 + x2*y3 + x3*y1 - x1*y3 - x2*y1 - x3*y2)/2.0 == 0.0

//Start = collinear { x = 0.0, y = 0.0} { x = 1.0 , y = 0.0 } { x = 3.0 , y = 0.0 } 
//Start = collinear { x = 0.0, y = -1.0} { x = 2.0, y = 0.0} { x = 3.0, y = 0.0} 


/*
Given a list of distinct name and a list of grades.
Generate a list of 'Person' type 
the grades of all Person should be the average of the 2nd list.
*/

:: Person = { name :: String , gpa :: Real } 

Generator :: [String] [Int] -> [Person]
Generator list1 list2 = map (\x = {name = x , gpa = y}) list1 

where 
  y = (toReal (sum list2)) / (toReal (length list2))
  
//Start = Generator ["p1","p2"] [1,4] //  

:: Student = { id :: Int , uni :: String, grades :: [Int] }

David :: Student 
David = { id = 111111 , uni = "ELTE" , grades = [2,3,4] }
David2 :: Student 
David2 = { id = 222222 , uni = "ELTE" , grades = [3,4,5] }
Peter :: Student 
Peter = {id = 123456 , uni = "ELTE" , grades = [1,2,3,1,3,4,2,3,4,5] }
Mark::Student
Mark = {id = 134326, uni = "BME", grades = [1,1,1,1,1,1,5,3,3,4,2,3,4,5] }
John :: Student
John = { id = 133526 , uni = "BME" , grades = [1,1,1,1,1,5,3,3,4,2,3,4,5] }
Sara :: Student 
Sara = { id = 444326 , uni = "BME" , grades = [1,2,5,5,5,2,2,3,4,2,3,4,5] }
Ana :: Student 
Ana = { id = 134326 , uni = "Corvious" , grades = [1,1,1,1,1,2,2,2,2,1,1,4,4,4,4,5,5,5,5] }
Leo :: Student 
Leo = { id = 555555, uni = "ELTE" , grades = [1,2,5,5,5,5,3,3,2,2,2,2,2,2,2,3,4,5] }
Jane :: Student 
Jane = { id = 134536, uni = "Corvious" , grades = [ 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2] }


/*
Given an array of students, return an array of the names of the
universities [ remove the duplicate names ]
*/

uniNames :: {Student} -> {String}
uniNames arr = { a.uni \\ a <-: arr }

//Start = uniNames {David,David2,Peter,Mark,John,Sara,Ana,Leo,Jane}

/*
Given an array of students. Find the id of the students with the lowest gpa.
*/

gpa :: [Int] -> Real
gpa list = (toReal (sum list))/(toReal (length list)) 

//Start = gpa [3,4,5]

lowestGpa :: {Student} -> Real
lowestGpa arr = maxList [ gpa (x.grades) \\ x <-: arr ]

//Start = lowestGpa {David,David2,Peter,Mark,John,Sara,Ana,Leo,Jane}


/*
Given an array of students and an integer n,
return a list of tuples like (id,uni) of the students that have more than n grades 
*/

moreN :: {Student} Int -> [(Int,String)]
moreN arr num = [ (x.id,x.uni) \\ x <-: arr | length (x.grades) > num ]

//Start = moreN {David,David2,Peter,Mark,John,Sara,Ana,Leo,Jane} 3


/*
Write a function which takes a university name and array of students and
calculate the total gpa of the university 
GPA of university is the sum of all the grades of all the student over the
total number of grades.
*/

getUni :: String {Student} -> Real
getUni name arr = (toReal (sum [ sum (a.grades) \\ a <-: arr | a.uni == name ])) / (toReal (sum [ length (a.grades) \\ a <-: arr | a.uni == name ] ))


//Start = getUni "ELTE" {David,David2,Jane,Ana,Sara}
//Start = getUni "ELTE" {Mark,Jane,Mark} 
 


/*
Given an array of students, returns a tuple containing the names of the universities with
the lowest and the highest gpa.

(LowestGpa,HighestGpa)
*/

minMaxUniAux :: {Student} -> [Real]
minMaxUniAux array = [ gpa (x.grades) \\ x <-: array ]
 
//Start = minMaxUniAux {David,David2,Jane,Ana,Sara}

minMaxUniAux2 :: {Student} -> (Real,Real)
minMaxUniAux2 array = (x,y)

 where x = minList (minMaxUniAux array)
       y = maxList (minMaxUniAux array)

//Start = minMaxUni {David,David2,Jane,Ana,Sara}

minMaxUni :: {Student} -> (String,String)
minMaxUni array = flatten [ (x.uni,y.uni) \\ x <-: array , y <-: array | gpa (x.grades) ==  minList (minMaxUniAux array) && gpa (x.grades) ==  maxList (minMaxUniAux array) ]
 
Start = minMaxUni {David,David2,Jane,Ana,Sara}























































