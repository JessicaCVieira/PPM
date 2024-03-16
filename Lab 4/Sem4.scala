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
val n = new Match (("Uk", 2), ("MU", 1))
val f: Fixtures = List(
  (("TeamA", 2), ("TeamB", 1)),
  (("TeamA", 3), ("TeamD", 3)),
  (("TeamE", 0), ("TeamF", 2))
)

//foldLeft
// a)
/*Retorna true se forem equipas diferentes
  Retorna false se for a mesma equipa no mesmo jogo*/
def noItselfL(f: Fixtures) : Boolean = 
    (f foldLeft true) ((acc, matchTuple) => acc && matchTuple._1._1 != matchTuple._2._1)

// b) 
def withoutRepL(f: Fixtures) : Boolean = 
{
    (f foldLeft true) (())
}

//foldRight
//a) 
/*def noItselfR(f: Fixtures) : Boolean = 
{
    
}*/
