import scala.annotation.tailrec

type Board = List[List[Cells.Cell]]

object Cells extends Enumeration { 
 type Cell = Value 
 val Circle, Cross, Empty = Value 
}


def indexList(lst:List[Cells.Cell], c:Cells.Cell): List[Int] = 
{
    @tailrec
    def aux(list: List[Cells.Cell], index: Int, acc: List[Int]) : List[Int] = 
    {
        list match
        {
            case Nil => acc
            case head :: tail => 
                if(head == c) aux(tail, index + 1, acc :+ index)
                else aux(tail, index + 1, acc)
        }
            
    }

    aux(lst, 0, Nil)  

}

/* Teste

scala> indexList(List(Cells.Circle,Cells.Empty,Cells.Cross, Cells.Circle),Cells.Circle)
val res4: List[Int] = List(0, 3)

*/


def count(lst1:List[Int], lst2:List[Int]):Int = {

    (lst1 foldRight 0) ((item, c) => 
        if (lst2.contains(item)) c + 1
        else c )

}

/* Teste

scala> count(List(2,0),List(0,1,2,3))
val res6: Int = 2

*/

