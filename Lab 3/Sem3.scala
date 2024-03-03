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

//Ex 3 correto
def ex3(a: List[Any], b: List[Any]) : List[Any] = {
    if(a.length != b.length) Nil
    a match{
        
    }

} 

