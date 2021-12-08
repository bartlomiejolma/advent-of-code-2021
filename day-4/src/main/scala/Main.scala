import scala.io.Source

val filename = "input.txt"

case class Board(board: Array[Array[Int]], var won: Boolean = false)  {
  def mkString(): String = {
    board.map(_.mkString(" ")).mkString("\n")
  }
  def sum(): Int = {
    board.flatten.sum
  }
}
case class Input(numbers: List[Int], boards: Array[Board])


def parseInput(input: String): Input = {
  val head::tail = input.split("\n\n").toList
  val numbers = head.split(",").map(_.toInt).toList
  val boards = tail.map(boardInput => Board(boardInput.split("\\n").map(row => row.trim.split("\\s+").map(_.toInt).toArray).toArray)).toArray
  Input(numbers, boards)
}

type BoardIndex = (Int, Int, Int)
type BoardWinIndex = (Int, Int)

type NumbersPositions = scala.collection.mutable.Map[Int, List[BoardIndex]]
type Counts = scala.collection.mutable.Map[BoardWinIndex, Int]

def getDataStructures(input: Input): Tuple3[NumbersPositions, Counts, Counts] = {

  val numbersPositions: NumbersPositions = scala.collection.mutable.Map[Int, List[BoardIndex]]()
  val countsRow = scala.collection.mutable.Map[BoardWinIndex, Int]()
  val countsCol = scala.collection.mutable.Map[BoardWinIndex, Int]()

  for (case (board, boardIndex) <- input.boards.zipWithIndex) {
    for (case (row, rowIndex) <- board.board.zipWithIndex) {
      for (case (entry, entryIndex) <- row.zipWithIndex) {
        numbersPositions.get(entry) match {
          case Some(positions) => numbersPositions += (entry -> (Tuple3(boardIndex, rowIndex, entryIndex)::positions))
          case None => numbersPositions += (entry -> List(Tuple3(boardIndex, rowIndex, entryIndex)))
        }
        countsRow += (Tuple2(boardIndex, rowIndex) -> 0)
        countsCol += (Tuple2(boardIndex, entryIndex) -> 0)
      }
    }
  }
  return (numbersPositions, countsRow, countsCol)
}

def partOneWon(boards: Array[Board], boardIndex:BoardIndex): Boolean = {
    println(s"$boardIndex board won by row")
    println(boards(boardIndex._1).mkString())
    return true
}

def partTwoWon(boards: Array[Board], boardIndex:BoardIndex): Boolean = {
    boards(boardIndex._1).won = true
    if (boards.forall(board => board.won)) {
      println(s"$boardIndex board won by col")
      println(boards(boardIndex._1).mkString())
      return true
    } 
    return false
}

type WonHandler = (Array[Board], BoardIndex) => Boolean


def runBingo (wonHandler: WonHandler)(input: Input, numbersPositions: NumbersPositions, countsRow: Counts, countsCol: Counts): Tuple2[Board, Int] = {

  for (number <- input.numbers) {
    val positions = numbersPositions(number)
    for (position <- positions) {
      countsRow(Tuple2(position._1, position._2)) += 1
      countsCol(Tuple2(position._1, position._3)) += 1
      input.boards(position._1).board(position._2)(position._3) = 0
      val boardWon = countsRow(Tuple2(position._1, position._2)) == 5 || countsCol(Tuple2(position._1, position._3)) == 5
      val shouldFinish = boardWon && wonHandler(input.boards, position)
      if (shouldFinish) {
        return (input.boards(position._1), number)
      }
    }
  }
  return (input.boards(0), -1)
}


def getResult(winner: (Board, Int) ): Int = {
  return winner._2 * winner._1.sum()
}

def partOne(text: String): Int = 

  val input = parseInput(text)

  val (numbersPositions, countsRow, countsCol) = getDataStructures(input)
    
  return getResult(runBingo(partOneWon)(input, numbersPositions, countsRow, countsCol))

def partTwo(text: String): Int = 

  val input = parseInput(text)

  val (numbersPositions, countsRow, countsCol) = getDataStructures(input)
    
  return getResult(runBingo(partTwoWon)(input, numbersPositions, countsRow, countsCol))

@main def hello: Unit = 
  def text = Source.fromFile(filename).mkString
  println(partOne(text))
  println(partTwo(text))
