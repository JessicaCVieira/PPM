
type Board = List[List[Cells.Cell]]

object Cells extends Enumeration { 
 type Cell = Value 
 val Circle, Cross, Empty = Value 
}


def indexList(lst:List[Cells.Cell], c:Cells.Cell): List[Int] = 
{
    def aux(list: List[Cells.Cell], index: Int, acc: List[Int]){
        
        list match
            case Nil => acc
            case head :: tail => 
    }
    
    
}
