module practice1
import StdEnv


::University={uniName::String,students::[Student],teachers::[Teacher]}
::Teacher={name::String,subject::String}
::Student={studentName::String,age::Int,grades::{Int},favoriteTeacher::Teacher}

ELTE::University
ELTE={uniName="ELTE",students=[Marko,Nikola,Josh,Dame],teachers=[Mary,Peter,John]}
BMI::University
BMI={uniName="BMI",students=[Ana,Josh,Sofi,Nikola],teachers=[Viktor,John,Peter]}
EmptyUni::University
EmptyUni={uniName="Empty",students=[],teachers=[]}

Peter::Teacher
Peter={name="Peter",subject="Functional"}
Viktor::Teacher
Viktor={name="Viktor",subject="Math"}
Mary::Teacher
Mary={name="Mary",subject="OOP"}
John::Teacher
John={name="John",subject="Functional"}

Marko::Student
Marko={studentName="Marko",age=19,grades={4,4,4,5},favoriteTeacher= Mary}
Sofi::Student
Sofi={studentName="Sofi",age=22,grades={5,5,4,5,5},favoriteTeacher=John}
Dame::Student
Dame={studentName="Dame",age=21,grades={2,3,4,5},favoriteTeacher=Peter}
Ana::Student
Ana={studentName="Ana",age=18,grades={5,5,5,5},favoriteTeacher=Viktor}
Nikola::Student
Nikola={studentName="Nikola",age=19,grades={4,4,4,4,2},favoriteTeacher=Peter}
Nik::Student
Nik={studentName="Nik",age=20,grades={4,4,4,4,3},favoriteTeacher=Peter}
Nik2::Student
Nik2={studentName="Nik2",age=22,grades={4,4,4,4,5},favoriteTeacher=Peter}
Josh::Student
Josh={studentName="Josh",age=22,grades={4,5,5},favoriteTeacher=John}



/*1 Given a University, return an array of all the 
students names which have gpa greater than 4, and a favorite teacher who teaches Functional*/

gpaAndFavoriteTeacher::University->{String}
gpaAndFavoriteTeacher uni = {x.studentName \\x<-uni.students |(toReal(sum [y\\y<-:x.grades]))/(toReal(length [y\\y<-:x.grades])) > 4.0 && x.favoriteTeacher.subject=="Functional"}
 

//Start=gpaAndFavoriteTeacher BMI//{"Josh","Sofi"}
//Start=gpaAndFavoriteTeacher ELTE//{"Josh"}
//Start=gpaAndFavoriteTeacher EmptyUni//{}

/*2 Given a University, return an array of all the 
students or teachers names which are shorter than 6*/

shorterThan6 :: University -> {String}
shorterThan6 uni = { b \\ b <- [x.studentName \\ x <- uni.students | length ( [y \\ y <-: x.studentName]) < 6 ] ++ [x.name \\ x <- uni.teachers | length ( [y \\ y <-: x.name]) < 6 ] }
//shorterThan6 uni=  {b\\b<-[y.studentName\\y<-(uni.students)|length[a\\a<-:y.studentName]<6]++[y.name\\y<-(uni.teachers)|length[a\\a<-:y.name]<6]}
 

//Start=shorterThan6 BMI//{"Ana","Josh","Sofi","John","Peter"}
//Start=shorterThan6 ELTE//{"Marko","Josh","Dame","Mary","Peter","John"}
//Start=shorterThan6 EmptyUni//{}

/*3 Write a function which will take an array of Universities and return the University with 
    the highest overall gpa (the average of the average of each student)*/
    
sgpa :: {Int} -> Real 
sgpa list = toReal (sum [ x \\ x <-: list ]) / toReal (length [ x \\ x <-: list ])

ugpa :: {Student} -> Real
ugpa list = maxList [ sgpa x.grades \\ x <-: list ]

//Start = ugpa {Dame,Ana,Josh}
    
highestGpa :: {University} -> String
highestGpa uni = {    y    \\ y <-: x.students \\ x <-: uni }
    
Start=highestGpa {ELTE,BMI,EmptyUni}//"BMI"
//Start=highestGpa {ELTE,BMI} //"BMI"
//Start=highestGpa {EmptyUni,EmptyUni}//"Empty"
//Start=highestGpa {ELTE} //"ELTE"
//Start=highestGpa {}//"No universities given"



















