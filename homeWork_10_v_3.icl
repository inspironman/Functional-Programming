module homeWork_10_v_3
import StdEnv


/* Write a function that takes a binary tree as an argument 
 * and converts it to the rose tree. Tree should maintain same
 * values and same structure. RoseTree and BinTree types are given
 * bellow. Assume that input binary tree contains at least one value.
 */
:: RoseTree a = RoseNode a [(RoseTree a)]
:: BinTree a = BinNode a (BinTree a) (BinTree a) | TreeEnd

bt1 = (BinNode 3 (BinNode 2 TreeEnd (BinNode 4 TreeEnd TreeEnd)) (BinNode 1 (BinNode 5 TreeEnd TreeEnd) TreeEnd))
bt2 = (BinNode 0 TreeEnd TreeEnd)
bt3 = (BinNode 10 (BinNode 11 bt1 bt2) (BinNode 12 TreeEnd bt2))

binToRoseTree :: (BinTree a) -> (RoseTree a)
binToRoseTree (BinNode x TreeEnd TreeEnd) = RoseNode x []
binToRoseTree (BinNode x l TreeEnd) = RoseNode x [binToRoseTree l]
binToRoseTree (BinNode x TreeEnd r) = RoseNode x [binToRoseTree r]
binToRoseTree (BinNode x l r) = RoseNode x ([binToRoseTree l] ++ [binToRoseTree r])

//Start = binToRoseTree bt1 // (RoseNode 3 [(RoseNode 2 [(RoseNode 4 [])]),(RoseNode 1 [(RoseNode 5 [])])])
                           //  (RoseNode 3 [(RoseNode 2 [(RoseNode 4 [])]),(RoseNode 1 [(RoseNode 5 [])])])

//Start = binToRoseTree bt2 // (RoseNode 0 [])
                           //  (RoseNode 0 [])


//Start = binToRoseTree bt3 // (RoseNode 10 [(RoseNode 11 [(RoseNode 3 [(RoseNode 2 [(RoseNode 4 [])]),(RoseNode 1 [(RoseNode 5 [])])]),(RoseNode 0 [])]),(RoseNode 12 [(RoseNode 0 [])])])
	                       //  (RoseNode 10 [(RoseNode 11 [(RoseNode 3 [(RoseNode 2 [(RoseNode 4 [])]),(RoseNode 1 [(RoseNode 5 [])])]),(RoseNode 0 [])]),(RoseNode 12 [(RoseNode 0 [])])])
				  

//=====================================================================================================================================================================================================

/*
Create an * instance of lists such that list1 * list2 will give a
list of the even numbers in each position
if both numbers are odd, delete them 

e.g: [1,2,3,4,5,7] 
///* [2,3,4,5,6,7] = [2,2,4,4,6]*/
*/
instance * [Int] 
where
	(*) l1 l2 = filter (isEven) [((\x y | isEven x = x = y) x y)\\ x<- l1 & y<-l2 ]



//Start = [1,2,3,4,5,7] * [2,3,4,5,6,7] // [2,2,4,4,6]
//Start = [1,5] * [2] // [2]
//Start = [1,5] * [] // []








