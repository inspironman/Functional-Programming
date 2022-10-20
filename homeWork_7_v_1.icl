module homeWork_7_v_1
import StdEnv


/* 1. Define a Person record which contains name and height two fields,
with type of String and Real respectively. Write a function which takes a person
and a certain height, if the person is taller than 1.70, subtract their height by
1%.

*/

::Person1 = { name1 :: String, tall :: Real}
John::Person1
John={name1 = "John", tall= 1.78}
Mike::Person1
Mike={name1 = "Mike", tall= 1.58}
Lily::Person1
Lily={name1 = "Lily", tall= 1.85}
 
 
ChangeHeight :: Person1 -> Person1

ChangeHeight p 
| p.tall > 1.70 = { name1 = p.name1 , tall = (p.tall - (p.tall*0.01)) }
= { name1 = p.name1 , tall = p.tall }


//Start = ChangeHeight John // (Person1 "John" 1.7622)
//Start = ChangeHeight Mike // (Person1 "Mike" 1.58)
//Start = ChangeHeight Lily // (Person1 "Lily" 1.8315)


 

::Person={name::String, mass::Real, height::Real, bmi::Real}
Rose::Person
Rose={name="Rose", mass=147.71, height=1.72, bmi=0.0}
Jack::Person
Jack={name="Jack", mass=158.73, height=1.93, bmi=0.0}
Emilia::Person
Emilia={name="Emilia", mass=121.25, height=1.60, bmi=0.0}
Leo::Person
Leo={name="Leo", mass=85.98, height=1.75, bmi=0.0}
Grace::Person
Grace={name="Grace", mass=112.43, height=1.65, bmi=0.0}
Harry::Person
Harry={name="Harry", mass=169.76, height=1.80, bmi=0.0}





/* 2.
Given an array of Persons, write a function that calculates the BMI of each Person
BMI: body mass index = m / h^2
m = mass (in kilograms)
h = height (in meters)
note: the mass given in the records are in pounds, you need to convert before using the formula
hint: 1 pound = 0.453592kg
*/

converter::{a}->[a]
converter arr = [a \\ a <-: arr]

converter2::[a]->{a}
converter2 list = {a \\ a <- list}

bmi p = {name = n, mass = m ,height = h*100.0, bmi = m/(h*h)}
where
    n = p.name
    m = toReal(toInt((p.mass)*(0.453592)))
    h = toReal(p.height)

calcBMI :: {Person} -> {Person}
calcBMI arr = converter2(map bmi (converter arr))

//Start = calcBMI {Rose,Jack,Emilia}  // {(Person "Rose" 67 172 22.6473769605192),(Person "Jack" 72 193 19.3293779698784),(Person "Emilia" 55 160 21.484375)}
                                     // {(Person "Rose" 67 172 22.6473769605192),(Person "Jack" 72 193 19.3293779698784),(Person "Emilia" 55 160 21.484375)}  
//Start = calcBMI {Leo,Grace,Harry} // {(Person "Leo" 39 175 12.734693877551),(Person "Grace" 51 165 18.732782369146),(Person "Harry" 77 180 23.7654320987654)}
                                   //  {(Person "Leo" 39 175 12.734693877551),(Person "Grace" 51 165 18.732782369146),(Person "Harry" 77 180 23.7654320987654)}

//Start = calcBMI {} // {}


 











