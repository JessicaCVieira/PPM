//Ex: 1
/*
def sum(xs: List[Int]) = (xs foldRight 0) (_ + _) 
def product(xs: List[Int]) = (xs foldRight 1) (_ * _)
//1.1
def orFoldRight(lst: List[Boolean]) : Boolean = (lst foldRight false) (_ || _)
def orFoldLeft(lst: List[Boolean]) : Boolean = (lst foldLeft false) (_ || _)
def andFoldRight(lst: List[Boolean]) : Boolean = (lst foldRight true) (_ && _)
import org.w3c.dom.Text
def andFoldLeft(lst: List[Boolean]) : Boolean = (lst foldLeft true) (_ && _)*/

//1.2
//remover duplicados consecutivos
def remDup[A](lst: List[A]) : List[A] = (lst foldRight List[A] () ) ((x, xs) => x::(xs.dropWhile(_ == x)))

//EX. 2 
type Team = String 
type Goals = Int 
type Match = ((Team, Goals), (Team, Goals)) 
type Fixtures = List[Match]

//a) 
// dá true se nenhuma equipa jogar contra ela msm
// dá falso se jogar c a msm equipa
def same(m: Match): Boolean = m._1._1.equals(m._2._1)
def noItself( f: Fixtures) : Boolean = 
{ 
  (f foldRight true) ((m1,m2)=> !same(m1) && m2)
}

// Teste a)
/*val fixtures1 = List((("A", 2), ("B", 1)), (("C", 3), ("C", 2)))
val fixtures2 = List((("X", 1), ("Y", 0)), (("Z", 2), ("W", 1)))
val fixtures3 = List((("M", 2), ("M", 1)), (("N", 3), ("O", 2)))
val fixtures4 = List((("A", 2), ("M", 1)), (("M", 3), ("O", 2)))

println(noItself(fixtures1)) // false (equipa C joga com C)
println(noItself(fixtures2)) // true (nenhuma partida é entre uma equipe e ela mesma)
println(noItself(fixtures3)) // false (M joga com M)
println(noItself(fixtures4)) // true*/

//b)
//verificar que nenhuma equipa joga mais do q 1 jogo
//ou seja não pode haver equipas repetidas na fixture
def existCount(t: Team, j:Fixtures): Int = 
{ 
 (j foldRight 0) ( (m1, m2)=> 
  if((t.equals(m1._1._1) || t.equals(m1._2._1))) 
    1+m2 
  else
  m2)
}

def withoutRep(j : Fixtures) : Boolean = 
{ 
 (j foldRight true) ( (m1,m2)=> 
  if(existCount(m1._1._1, j)>1 || existCount(m1._2._1, j)>1) 
    false
  else
  m2)
}

def teams(j: Fixtures) : List[Team] = 
{
  (j foldLeft List[Team] () ) ((acc, m) => acc :+ (m._1._1) :+ (m._2._1))
}

/*def teams(j: Fixtures): List[Team] = { (j foldRight List[Team]()) 
((m,j) => (m._1._1):: (m._2._1)::j)}
*/

//dá true se empatou
def empate(m: Match) : List[(Team, Team)] = 
{
  if (m._1._2 == m._2._2) List((m._1._1, m._2._1))
  else List()
}

def draws(j: Fixtures) : List[(Team, Team)] = 
{
  (j foldLeft List[(Team, Team)]()) ((acc, m) => acc ++ empate(m) ) 
}

//prof
/*def draws(j: Fixtures): List[(Team, Team)] = 
{ 
 (j foldRight List[(Team, Team)]()) ( (x, j) => 
  if(x._1._2 == x._2._2) 
    ((x._1._1),(x._2._1))::j 
 else
  j) 
}*/

def points(j: Fixtures) : List[(Team, Int)] = 
{
  (j foldRight List[(Team, Int)]()) ((x, j) => 

    // Empate
    if(x._1._2 == x._2._2)
      ((x._1._1), 1) :: ((x._2._1), 1) :: j

    // 1º ganha
    else if(x._1._2 > x._2._2)
      ((x._1._1), 3) :: ((x._2._1), 0) :: j

    // 1º perde
    else 
      ((x._1._1), 0) :: ((x._2._1), 3) :: j
  )
}


