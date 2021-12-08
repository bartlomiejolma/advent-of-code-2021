import scala.io.Source

val filename = "input.txt"

case class Board(board: Array[Array[Int]]) {
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

 
def partOne(text: String): Int = 

  val input = parseInput(text)

  val numbersPositions = scala.collection.mutable.Map[Int, List[Tuple3[Int, Int, Int]]]()
  val countsRow = scala.collection.mutable.Map[Tuple2[Int, Int], Int]()
  val countsCol = scala.collection.mutable.Map[Tuple2[Int, Int], Int]()

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
  for (number <- input.numbers) {
    val positions = numbersPositions(number)
    for (position <- positions) {
      countsRow(Tuple2(position._1, position._2)) += 1
      countsCol(Tuple2(position._1, position._3)) += 1
      input.boards(position._1).board(position._2)(position._3) = 0
      if (countsRow(Tuple2(position._1, position._2)) == 5) {
        println(s"$position board won by row")
        println(input.boards(position._1).mkString())
        return number * input.boards(position._1).sum()
      }
      if (countsCol(Tuple2(position._1, position._3)) == 5) {
        println(s"$position board won by col")
        println(input.boards(position._1).mkString())
        return number * input.boards(position._1).sum()

      }
    }
  }
  return 0

@main def hello: Unit = 
  def text = Source.fromFile(filename).mkString
  println(partOne(text))
