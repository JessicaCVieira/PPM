
//Ex 4
//a) filtering 
def paresord(lst: List[(Int, Int)]) : List[(Int, Int)] = 
{
    lst match {
        case Nil => Nil
        case head :: tail => 
            if(head._1 < head._2) head :: paresord(tail)
            else paresord(tail)
    }
}

//b) folding
def myconcat(lst: List[String]) : String = 
{
    lst match{
        case Nil => " "
        case List(_) => lst.head
        case head :: tail => head + " " + myconcat(tail)
    }
}

//c) mapping (apply max on all tupples)
def maximum(lst : List[(Double, Double)]) : List[Double] = 
{
    lst match{
        case Nil => Nil 
        case head :: tail => 
            if (head._1 > head._2) head._1 :: maximum(tail)
            else head._2 :: maximum(tail)
    }
}