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
        case _ => l :+ e                      //TODO -> Ver dif entre :: e :+
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




