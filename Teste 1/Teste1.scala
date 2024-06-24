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

// outra resolução
def nInLine(col: List[Cells.Cell], c: Cells.Cell, n: Int): Boolean = {
  @tailrec
  def nInLine1(col: List[Cells.Cell], c: Cells.Cell, n: Int, acc: Int): Boolean = {
    col match {
      case Nil => acc >= n
      case head :: tail =>
        if (head == c)
          if(acc + 1 == n) true
          else nInLine1(tail, c, n, acc + 1)
        else nInLine1(tail, c, n, 0) // o acc é zero porque queremos encontrar seguidos, logo se não encontramos o contador começa do zero
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


//outra resolução
def winner(b: Board, player: Cells.Cell): Boolean = {
  
  def linesWinner(b: List[List[Cells.Cell]], player: Cells.Cell): Boolean = {
    b match
      case Nil => false
      case head :: next => 
        nInLine(head, player, 4) || linesWinner(next, player)
  }

  val transposto = transpose(b)

  val linhas = linesWinner(b, player)
  val colunas = linesWinner(transposto, player)

  linhas || colunas

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

//println(winner(board, Cells.Red)) // Deve retornar true
//println(winner(board, Cells.Blue)) // Deve retornar false

def columnsWithEmptyCells(b: Board): List[Int] = {
  val transposedBoard = transpose(b).zipWithIndex
  (transposedBoard foldRight List[Int]()) { case ((column, index), acc) =>
    if (column.contains(Cells.Empty)) index :: acc
    else acc
  }
}

def columnsWithEmptyCells(b: Board): List[Int] = {

  val transposedBoard = transpose(b)
  
  def hasEmpty(row: List[Cells.Cell]) : Boolean = {
    (row foldRight false) ((cell, acc) => cell == Cells.Empty || acc)
  }

  def col(b: List[List[Cells.Cell]], acc:List[Int], index: Int): List[Int] = {
    b match
      case Nil => acc
      case head :: next => 
        if(hasEmpty(head) == true) col(tail, acc :+ index, index + 1)
        else col(tail, acc, index + 1)
  }

  col(transposedBoard, Nil, 0)

}


//teste
/*println(columnsWithEmptyCells(board))

class Controller 
{
  @FXML
  var label1: Label = _
  def onMouseClicked(): Unit = 
  {
    if (App.game.winner(Cells.Blue)) label1.setText("You won!")
  }
}
*/


//teste 2
def numberOfPlays(b:Board): ((Cells.Cell, Int), (Cells.Cell, Int))= 
{
  //percorrer o tabuleiro todo por cada celula não vazia se for azul conta para azul, se for vermelha conta para vermelha
  def countRow(row: List[Cells.Cell], c: Cells.Cell) : Int = {
    row.count(_ == c)
  }

  @tailrec
  def countPlays(rows: List[List[Cells.Cell]], red: Int, blue: Int): (Int, Int) ={
    rows match{
      case Nil => (red, blue)
      case head :: next => 
        val redAux = countRow(head, Cells.Red)
        val blueAux = countRow(head, Cells.Blue)
        countPlays(next, red + redAux, blue + blueAux)
    }    
  }

  val (redT, blueT) = countPlays(b, 0, 0)
  ((Cells.Red, redT), (Cells.Blue, blueT))
}

def play(board:Board, player:Cells.Cell, col:Int): Board = 
{
 //transpor tabuleiro: Linhas passam a colunas
 val trasposto = transpose(board)

 //substituir a primeira céluna vazia
 def replaceEmpty(row: List[Cells.Cell], player: Cells.Cell) : List[Cells.Cell] = {
  
  row match{
    case Cells.Empty :: next => player :: next
    case head :: next => head :: replaceEmpty(next, player)
    case Nil => Nil
  }
 }

 val updateColuna = replaceEmpty(trasposto(col), player)
 val updateBoard = trasposto.updated(col, updateColuna) 

 transpose(updateBoard)
  
}


//células vazias numa linha (a coluna anteriormente)
def countEmptyCol(row: List[Cells.Cell]) : Int = {
  (row foldRight 0) ((r, c) => if(r == Cells.Empty) c + 1 else c)
}

def numberEmptyCellsByColumn(b:Board) : List[(Int, Int)] = {

  val colunasLinhas = transpose(b)

  def loop(colunas: List[List[Cells.Cell]], index: Int, acc: List[(Int, Int)]): List[(Int, Int)] = {
    colunas match {
      case Nil => acc
      case head :: next => 
        val emptyCount = countEmptyCol(head)
        loop(next, index + 1, acc :+ (index, emptyCount))
    }
  } 

  loop(colunasLinhas, 0, Nil)

}

class Controller {

  @FXML

  var boardCell00: Circle = _

  def onMouseClicked(): Unit = {

    if(App.game.isEmptyPosition(0,0)) boardCell00.setFill(Color.RED)

  }
}

//Sopa de letras 
def fillOneCell(board: Board, letter: Char, coord: Coord2D): Board = {

  def fillRow(b: List[List[Char]], l: Char, c: Coord2D, index: Int): Board = {
    b match
      case Nil => b
      case head :: next => 
        if (index == c._1)
          fillPosition(head, l, c, 0) :: tail
        else 
          head :: fillRow(tail, l, c, index + 1)
    
  }

  def fillPosition(row:List[Char], l: Char, c: Coord2D, index: Int) : List[Char] = {
    row match
      case Nil => row
      case head :: next => 
        if(index == c._2)
          l :: tail
        else 
          head :: fillPosition(tail, l, c, index + 1)    
  }

  fillRow(board, letter, coord, 0)

}

//teste 2
def numberOfPlays(b:Board): ((Cells.Cell, Int), (Cells.Cell, Int)) = {

  // conta o tipo de uma dada celula numa dada linha
  def numPerLine(line: List[Cells.Cell], c: Cells.Cell) : Int = {
    line.count(_ == c)
  }
    
  @tailrec
  def countTotal (b: List[List[Cells.Cell]], red: Int, blue: Int) : (Int, Int) = {
    b match
      case Nil => (red, blue)
      case head :: tail =>
        countTotal(tail, red + numPerLine(head, Cells.Red), blue + numPerLine(head, Cells.Blue)) 
  }

  val(redT, blueT) = countTotal(b, 0, 0)
  ((Cells.Red, redT), (Cells.Cell, blueT))

}

def play1(board:Board, player:Cells.Cell, col:Int): Board = {

  val transposto = transpose(b)

  def replace(row: List[Cells.Cell], player: Cells.Cell) : List[Cells.Cell] = {
    row match
      case Cells.Empty :: tail => player :: tail
      case head :: tail => head :: replace(tail, player)
      case Nil => Nil
  }

  val updatedLine = replace(transposto(col), player)
  val final = transposto.updated(col, updatedLine)

  transpose(final)

}

def numberEmptyCellsByColumn(b:Board): List[(Int, Int)] = {

  val transposto = transpose(b)

  //contar empty por linha (por coluna)

  def numEmpty(line: List[Cells.Cell]) : Int = {
    (line foldRight 0) ((c, count) => 
      if(c == Cells.Empty) count + 1 else count)
  }

  def lineEmpty(rows: List[List[Cells.Cell]], index: Int) : List[(Int, Int)] = {
    rows match
      case head :: next => 
        (index, numEmpty(head)) :: lineEmpty(next, index + 1)
      case Nil => Nil
  }

  lineEmpty(transposto, 0)

}

