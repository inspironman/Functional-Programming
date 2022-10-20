module records 
import StdEnv

distance :: (Int,Int) (Int,Int) -> Real
distance (x1,y1) (x2,y2) = sqrt ( toReal ( (x1-x2)^2 + (y1-y2)^2 ) )


//Start = distance (3,4) (0,0)

/*
:: Point = { x :: Int , y :: Int }

//Start = { x = 3,y = 4}

origin :: Point
origin = { x = 0, y = 0}

p1 :: Point
p1 = {x = 3, y = 4}
 
p2 :: Point
p2 = {x = 6, y = 8} 
 
distance1 :: Point Point -> Real
distance1 o p = sqrt ( toReal ( (o.x-p.x)^2 + (o.y-p.y)^2 ) )

//Start = distance1 origin p1

instance - Point 
where 
     (-) a b = {x = a.x-b.x , y = a.y- b.y }
    
//Start = p1 - origin

instance + Point 
where 
     (+) a b = {x = a.x + b.x , y = a.y + b.y }
    
//Start = p1 + p2

instance == Point where (==) a b = a.x == b.x && a.y == b.y

//Start = isMember origin [p2,p1,p2]
//Start = isMember {x = 3, y = 4} [p2,p1]
 
*/

:: Point a = { x :: a , y :: a }

//Start = { x = 3,y = 4}

origin :: ( Point Int )
origin = { x = 0, y = 0}

p1 :: ( Point Int)
p1 = {x = 3, y = 4}
 
p2 :: ( Point Real )
p2 = {x = 6.23, y = 8.44} 
 
distance1 :: ( Point anyType) ( Point anyType ) -> Real | toReal anyType
distance1 o p = sqrt ( ( (x1-x2)^2.0 + (y1-y2)^2.0 ) )
where 
     x1 = toReal o.x
     x2 = toReal p.x
     y1 = toReal o.y
     y2 = toReal p.y

//Start = distance1 origin p1

instance - ( Point a ) | - a
where 
     (-) a b = {x = a.x-b.x , y = a.y- b.y }

//Start = p1 - p2

::Circle a = { center :: (Point a) , radius :: a} 
  
c1 :: ( Circle Int)
c1 = { center = p1 , radius = 5 }

c2 :: ( Circle Real)
c2 = { center = p2 , radius = 5.0 }

c3 :: ( Circle Real)
c3 = { center = p2 , radius = 6.2 }


//Start = c1.center.x 
/*

overlap :: (Circle a) (Circle a) -> Bool | toReal a 
overlap one two = dist > maxRad
where 
     dist = distance1 (one.center) (two.center)
     maxRad = max[one.radius, two.radius]
     
Start = overlap c2 c3
     
*/

::Date = { month :: Int, day :: Int}
:: Person = { name :: String, age ::Int , birthday::Date}

Evan :: Person
Evan = {name = "Evan", age = 1 , birthday ={month = 12, day = 9}}
Hossan :: Person
Hossan = { name = "Hossan" , age =69, birthday = {month = 3, day = 12}}

Tringa :: Person
Tringa = { name = "Tringa" , age =55, birthday = {month = 11, day = 22}}


instance == Date
where 
     (==) a b = a.month ==b.month && a.day == b.day

instance == Person 
where 
     (==) a b = a.name == b.name && a.age == b.age && a.birthday == b.birthday
     
instance < Person 
where 
     (<) a b =  a.age < b.age  

//Start = Evan == Evan

//Start = sort[Evan,Tringa,Hossan]

newBirth :: Person -> Person
newBirth a = { a & age = a.age+1}

//Start = newBirth Evan

instance toString Person
where
    toString a = a.name
    
Start = toString Evan

instance toInt Person 
where 
    toInt a = a.age






























































     






































































