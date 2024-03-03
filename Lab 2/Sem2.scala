def empty[A]( lst: List[A]) = lst.length == 0

def length[A]( lst: List[A]): Int = 
{
    lst match {
        case Nil => 0
        case _ :: tail => 1 + length(tail)
    }
}

def distance[A](lstPoint: List[(Double, Double)]): List[Double] = 
{
    lstPoint match {
        case Nil => Nil; 
        case (x, y) :: tail => (Math.sqrt(x * x + y * y)) :: 
        distance(tail); 
    } 
}

//Ex: 1 a)
def transf[A](lst: List[A]):List[A] = 
{
    lst match {
        case Nil => Nil 
        case List(_) => lst
        case head::second::tail=>second::head::transf(tail)
    }
}

//Ex: 1 b) 
def prod (lst: List[Double]) : Double = 
{
    lst match {
        case Nil => 0
        case List(e) => e
        case head :: tail => head * prod(tail)
    }
} 

//Ex 1 c) 
def add[A](l: List[A], e: A) : List[A] = 
{
    l match {
        case Nil => List(e)
        case _ => l :+ e                      // -> x :: xs - x primeiro e depois xs -> x :+ s - adiciona s no fim
    }
}

//1 d)
def concat[Any](a: List[Any], b: List[Any]) : List[Any] = 
{
    a match {
        case Nil => b
        case h :: t => h :: concat(t, b)
    }
}

//1 e)
def sumEl(lst: List[(Int, Int)]) : Int = 
{
    def sumHelper(lst: List[(Int, Int)], index: Int, sum: Int): Int = {
        lst match {
            case Nil => sum
            case head :: tail => 
                if (index == 2 || index == 4) sumHelper(tail, index + 1, sum + head._1 + head._2)
                else sumHelper(tail, index + 1, sum)
    }
  }
  sumHelper(lst, 0, 0)
}

//1 f) 
//i. 
//touple's first element = sum
//touple's second element = length
def helper1f (lst: List[Double]): (Double, Double) = 
{
    lst match {
        case Nil => (0,0)
        case List(_) => (lst.head, 1)
        case head :: tail => (head + (helper1f(tail)._1), 1 + (helper1f(tail)._2)) 
    }
}
//ii.
def average(lst: List[Double]) : Double = helper1f(lst)._1 / helper1f(lst)._2


//we can use val cannot use var

//1 g)
def ex1g(lst: List[Double], a: Double) : (List[Double], List[Double]) = 
{
    lst match{
        case Nil => (Nil , Nil)
        case head :: tail => 
            if(head <= a ) (head :: ex1g(tail, a)._1, ex1g(tail, a)._2) 
            else (ex1g(tail, a)._1, head :: ex1g(tail, a)._2)
    }
}

//1 h)
def biggerThanAverage(lst: List[Double]) : List[Double] = 
{
    val av = average(lst)
    def aux (l: List[Double], av: Double) : List[Double] = {
        l match{
        case Nil => Nil
        case head :: tail => 
            if (head >= av) head :: aux(tail, av)
            else aux(tail, av)
        }
    }
    aux(lst, av)
}

//Ex 2
type Entry = (String, String, String) 
type LTelef = List[Entry]

def emails(lst : LTelef) : List[String] = { 
    lst match { 
        case Nil => Nil  
        case (_ , _ , email):: tail => email :: (emails(tail)) 
    } 
}

val data: LTelef = List(
  ("John", "245687564", "john@example.com"),
  ("Jane", "95646", "jane@example.com"),
  ("Alice", "5464", "alice@example.com")
)

//2 i)
def emailsFromNumber(lst : LTelef) : List[String] = { 
    lst match { 
        case Nil => Nil  
        case (_ , number , email):: tail => 
            if (number.charAt(0) == '2') email :: (emailsFromNumber(tail))
            else (emailsFromNumber(tail))
    } 
}

//2 j) 
def infoFromName(lst : LTelef, a: String) : List[(String, String)]  = { 
    lst match { 
        case Nil => Nil  
        case (name, number , email):: tail => 
            if (name == a) (number, email) :: infoFromName(tail, a)
            else infoFromName(tail, a)
    } 
}



