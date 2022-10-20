module homeWork_8_v_4
import StdEnv


module homework_8
import StdEnv




/*
1.
    * Consider following example:
    * "aaaabbbcaddd" -> [(4, 'a'), (3, 'b'), (1, 'c'), (1, 'a'), (3, 'd')]
    * i.e. consecutive duplicates are grouped together and their count is taken in a tuple
    * together with the character.
    * This method is called Run-length encoding.
    * Your task is to implement "encode" function which does this.
*/

encode :: String -> [(Int,Char)]

// Start = encode "aaaabbbcaddd" // [(4, 'a'), (3, 'b'), (1, 'c'), (1, 'a'), (3, 'd')]
// Start = encode "" // []
// Start = encode "aa" // [(2, 'a')]
// Start = encode "abcde" // [(1, 'a'), (1, 'b'), (1, 'c'), (1, 'd'), (1, 'e')]








/*
 * Consider the Employee2 type, every employee reports back to a manager
 * There is a hierarchy in which everyone reports back to a manager until we reach the CEO
 * of the whole company. The CEO reports to no one.
*/

:: Manager =  Reports Employee2 | None

:: Employee2 = {name::String, manager :: Manager ,salary::Int}
 

emp1 :: Employee2 
emp1 = {name = "Abood", manager = None, salary = 100}
 
emp2 :: Employee2 
emp2 = {name = "Ali", manager = (Reports emp1), salary = 500}
 
emp3 :: Employee2 
emp3 = {name = "Saleh", manager = (Reports emp1) , salary = 250}

emp4 :: Employee2 
emp4 = {name = "Othman", manager = (Reports emp3), salary = 999}

emp5 :: Employee2 
emp5 = {name = "Mohammed", manager = (Reports emp3), salary = 1000}

emp6 :: Employee2 
emp6 = {name = "Ahmed", manager = (Reports emp4), salary = 900}


/*
 * Given two employees, write a function to find the closest manager, which both employees report to, and returns their name.

 * e.g: whoIsOurBoss emp5 emp4 should return the name Saleh since in the hierarchy, both emp5 and emp4 report back to emp3
*/
whoIsOurBoss :: Employee2 Employee2 -> String

// Start = whoIsOurBoss emp3 emp2 //  "Ali"
// Start = whoIsOurBoss emp4 emp5 // "Saleh"
// Start = whoIsOurBoss emp5 emp6 //  "Saleh"
// Start = whoIsOurBoss emp2 emp6 // "Abood" 

