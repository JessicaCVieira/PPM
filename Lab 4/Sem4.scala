//Ex: 1
/*
def sum(xs: List[Int]) = (xs foldRight 0) (_ + _) 
def product(xs: List[Int]) = (xs foldRight 1) (_ * _)
//1.1
def orFoldRight(lst: List[Boolean]) : Boolean = (lst foldRight false) (_ || _)
def orFoldLeft(lst: List[Boolean]) : Boolean = (lst foldLeft false) (_ || _)
def andFoldRight(lst: List[Boolean]) : Boolean = (lst foldRight true) (_ && _)
def andFoldLeft(lst: List[Boolean]) : Boolean = (lst foldLeft true) (_ && _)*/


//EX. 2 

type Team = String 
type Goals = Int 
type Match = ((Team, Goals), (Team, Goals)) 
type Fixtures = List[Match]

//test val
val m = new Match (("Uk", 2), ("Uk", 1))

//a) 
//returns false if a team plays with itself
def noItself(m: Match) : Boolean = 
{
    return !((m._1)._1 == (m._2)._1)
}

//b) 
//return false if a team plays more than 1 game
/*def withoutRep(f: Fixtures) : Boolean = 
{
    f match{
        case Nil => true
        case 
    }
}*/

//c) 
def teams(f: Fixtures) : List[Team] = 
{
    f match{
        case Nil => Nil
        case head => (head.head._1._1) :: (head.head._2._1)
        case head :: tail => (head._1._1) :: (head._1._2) :: teams(tail)
    }
}