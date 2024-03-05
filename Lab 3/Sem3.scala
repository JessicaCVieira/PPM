import scala.annotation.tailrec

object Ex1{

    def factorial1(n : Int):Int ={
        n match {
            case 0 => 1
            case _ => n*factorial1(n-1)
        }
    }

    def factorial2(n : Int) : Int = {
        @tailrec
        def factorialAux(n: Int, accum:Int) : Int = {
            if(n == 0) accum
            else factorialAux(n-1, accum*n)
        }
        factorialAux(n, 1)
    }

    def main (args: Array[String]): Unit = {
        println("F1:" + factorial1(5))
        println("F1:" + factorial2(5))
    }
        
}


    def remDup1(l : List[Any]) : List[Any] = {
        l match {
            case Nil => Nil
            case List(head) => List(head)
            case head::second::tail if head == second => remDup1(head::tail)
            case head::second::tail if head!=second => head::remDup1(second::tail)
        }
        
    }

    def remDup2(l : List[Any]) : List[Any] = {
        @tailrec
        def remDupAux[Any](l: List[Any], accum: List[Any]):List[Any]=
            l match{
                case Nil => accum
                case List(head) => accum:::List(head)
                case head::second::tail if head == second => remDupAux(second::tail, accum)
                case head::second::tail if head!=second => remDupAux(second::tail, accum:::List(head))
            }
        remDupAux(l, Nil)
    }




//Ex2

def lazyListRange(lo: Int, hi: Int): LazyList[Int] = {
    println(lo)
    if(lo >= hi) LazyList.empty
    else lo #:: lazyListRange(lo+1, hi)
}
def listRange(lo: Int, hi: Int): List[Int] = {
    println(lo)
    if(lo >= hi) List()
    else lo :: listRange(lo+1, hi)
}

//Ex 3 
// a)
def ex3(a: List[Int], b: List[Int]) : List[Int] = 
{
    if(a.length != b.length) Nil
    def loop(a1: List[Int], b1: List[Int], n: Int) : List[Int] = 
    { 
        a1 match{
            case Nil => Nil
            case x :: xs => x + b1.head :: loop(xs, b1.tail, n-1)
        }
    }
    loop(a, b, a.length)
} 

//mais simples (ñ era preciso o loop)
def ex3a(a: List[Int], b: List[Int]) : List[Int] = 
{
    if(a.length != b.length) Nil
    else 
        a match{
            case Nil => Nil
            case x :: xs => x + b.head :: ex3a(xs, b.tail)
        } 
} 

//b) with loop 
def zipWith[A, B, C](op: (A, B) => C, a: List[A], b: List[B]) : List[C] = 
{
    if(a.length != b.length) Nil
    def loop(a1: List[A], b1: List[B], n: Int) : List[C] = 
    { 
        a1 match{
            case Nil => Nil
            case x :: xs => op(x, b1.head) :: loop(xs, b1.tail, n-1)
        }
    }
    loop(a, b, a.length)
}

//b) without loop
def zipWith2[A, B, C](op: (A, B) => C, a: List[A], b: List[B]) : List[C] = 
{
    if(a.length != b.length) Nil
    else
        a match{
            case Nil => Nil
            case x :: xs => op(x, b.head) :: zipWith2(op, xs, b.tail)
        }
}

//c)
def isSorted[A](lst: List[A], ordered: (A,A) => Boolean): Boolean = 
{
    lst match {
        case Nil => true // the teacher does not especify what to do in this case
        case List(_) => true // the teacher does not especify what to do in this case
        case head :: second :: tail => ordered(head,second) && isSorted(tail, ordered)
    }
}

//d)
def bubbleSort(data: List[Int], f: (Int, Int) => Boolean): List[Int] = 
{
    def sort(lst: List[Int]) : List[Int] = 
    {
        lst match 
        {
            case Nil => Nil
            case List(_) =>  lst
            case head :: second :: tail => 
                if (f(head, second)) head :: sort(second :: tail) 
                else second :: sort(head :: tail)
        }
    }

    def loop (lst: List[Int], n: Int) : List[Int] = 
    {
        if (n <= 0) lst
        else loop(sort(lst), n - 1)        
    }   

    loop(data, data.length)
}

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

