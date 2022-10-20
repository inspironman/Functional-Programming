module ex18_solved
import StdEnv, StdDebug

//// Records

:: Point = {  x       ::  Real
            , y       ::  Real
            , visible ::  Bool
            }

:: Vector = { dx       ::  Real
            , dy       ::  Real
            }
  
Origo :: Point
Origo = { x = 0.0
        , y = 0.0
        , visible = True
        }
Dist :: Vector
Dist = { dx = 1.0
       , dy = 2.0
       }

IsVisible :: Point -> Bool
IsVisible {visible = True} = True
IsVisible _                = False

xcoordinate :: Point -> Real
xcoordinate p = p.x

hide :: Point -> Point
hide p = { p & visible = False }

Move :: Point Vector -> Point
Move p v = { p & x = p.x + v.dx, y = p.y + v.dy } 

//Start = Move (hide Origo) Dist

//// Trees

:: Tree a = Node a (Tree a) (Tree a)
          | Leaf

treesort :: ([a]-> [a]) | Eq, Ord a
treesort = collect o listtoTree

listtoTree :: [a] -> Tree a | Ord, Eq a
listtoTree [] = Leaf
listtoTree [x:xs] = insertTree x (listtoTree xs)

insertTree :: a (Tree a) -> Tree a | Ord a
insertTree e Leaf = Node e Leaf Leaf
insertTree e (Node x le ri)
   | e<=x = Node x (insertTree e le) ri
   | e>x  = Node x le (insertTree e ri)

collect :: (Tree a) -> [a]
collect Leaf = []
collect (Node x le ri) = collect le ++ [x] ++ collect ri

//Start = treesort [3, 1, 5, 9, 2, 7, 0]

nrT x y = length [1 \\ a<-x | a==y]

f :: (Tree Int) Int -> Int
f t a = nrT (collect t) a

//Start = f aTree 1
// Exercises

// 1. Compute the sum of the numbers placed in the nodes of a tree.
sumT :: (Tree Int) -> Int
sumT Leaf = 0
sumT (Node x le ri) = x + sumT le + sumT ri

aTree = Node 4 (Node 1 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) Leaf
// 4 + 2 +1 + 0 +0+ 3+0+0+0 = 10
//Start = aTree
//Start = sumT aTree

searchT :: (Tree Int) Int -> Bool
searchT Leaf a = False
searchT (Node x le ri) a
| x == a = True
=  searchT le a || searchT ri a

//Start = searchT aTree 10

// 2. Test about 3 points if they can form a right-angled triangle.
IsTriangle :: Point Point Point -> Bool
IsTriangle p1 p2 p3 = a == (b + c) || b == (a + c) || c == (a + b)
where
  a = (p2.x-p1.x)*(p2.x-p1.x) + (p2.y-p1.y)*(p2.y-p1.y)
  b = (p3.x-p2.x)*(p3.x-p2.x) + (p3.y-p2.y)*(p3.y-p2.y)
  c = (p3.x-p1.x)*(p3.x-p1.x) + (p3.y-p1.y)*(p3.y-p1.y)

//Start = IsTriangle Origo {x = 0.0, y = 3.0, visible = True} {x = 2.0, y = 0.0, visible = True}


// 3. Write another sort algorithm for sorting a list

ssort :: [Int] -> [Int]
ssort [] = []
ssort x = ssort l ++ (repeatn n m) //[m, m, m ..., m]
  where m = maxList x 
        n = (length x) - (length l)
        l = [y \\ y <- x | y <> m]

Start = (ssort [2,1,3,4,1,3,2,5,2,7])

bubble :: [Int] -> [Int] 
bubble []=[]
bubble [x]=[x]
bubble [x,y:xs]
|y>x = [x: bubble ([y] ++ xs)]
= bubble [y: bubble ([x] ++ xs)]

//Start = bubble [5,0,4,3,6,10,2,1]

