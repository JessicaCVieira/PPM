import scala.annotation.tailrec



object Cells extends Enumeration { 
  type Cell = Value
  val Red, Blue, Empty = Value 
}

type Board = List[List[Cells.Cell]]

def nInLine(col: List[Cells.Cell], c: Cells.Cell, n: Int): Boolean = {
  @tailrec
  def nInLine1(col: List[Cells.Cell], c: Cells.Cell, n: Int, acc: Int): Boolean = {
    col match {
      case Nil => acc >= n
      case head :: tail if head == c =>
        if (acc + 1 == n) true
        else nInLine1(tail, c, n, acc + 1)
      case _ :: tail => nInLine1(tail, c, n, 0)
    }
  }
  nInLine1(col, c, n, 0)
}

def transpose[B](b: List[List[B]]): List[List[B]] = {
  if (b.head.isEmpty) Nil
  else b.map(_.head) :: transpose(b.map(_.tail))
}

def winner(b: Board, player: Cells.Cell): Boolean = {
  def checkRows(board: Board): Boolean = {
    board.exists(row => nInLine(row, player, 4))
  }

  def checkCols(board: Board): Boolean = {
    checkRows(transpose(board))
  }

  checkRows(b) || checkCols(b)
}

// Teste do código
val board: Board = List(
  List(Cells.Red, Cells.Blue, Cells.Red, Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty),
  List(Cells.Red, Cells.Blue, Cells.Red, Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty),
  List(Cells.Red, Cells.Blue, Cells.Red, Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty),
  /*List(Cells.Red, Cells.Empty, Cells.Red, Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty),
  List(Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty),
  List(Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty)*/
)

println(winner(board, Cells.Red)) // Deve retornar true
println(winner(board, Cells.Blue)) // Deve retornar false

def columnsWithEmptyCells(b: Board): List[Int] = {
  val transposedBoard = transpose(b).zipWithIndex
  (transposedBoard foldRight List[Int]()) { case ((column, index), acc) =>
    if (column.contains(Cells.Empty)) index :: acc
    else acc
  }
}

//teste
println(columnsWithEmptyCells(board))

class Controller 
{
  @FXML
  var label1: Label = _
  def onMouseClicked(): Unit = 
  {
    if (App.game.winner(Cells.Blue)) label1.setText("You won!")


  }
}