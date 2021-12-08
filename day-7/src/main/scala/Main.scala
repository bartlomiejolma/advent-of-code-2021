import scala.io.Source

val filename = "input.txt"

def parseInput(input: String): Array[Int] = {
  input.split(",").map(_.toInt)
}

def solve(text: String): Int = {
  val positions = parseInput(text)
  val median = positions.sortWith(_ < _).splitAt(positions.length / 2)._2.head
  val mean = positions.sum / positions.length
  positions.map(x => Math.abs(x - median)).sum

}
def partOne(text: String): Int = solve(text)


def cost(n: Int): Int  = n * (n + 1) / 2


def costs(positions: Array[Int], align: Int): Int = {
  positions.map(x => cost((x - align).abs)).sum
}

def solve2(text: String): Int = {
  val positions = parseInput(text)
  val mean = (positions.sum / positions.length.toFloat).round.toInt

  println(costs(positions, mean))
  println(costs(positions, mean + 1))
  println(costs(positions, mean - 1))

  positions.map(x => cost((x - mean).abs)).sum

}

def partTwo(text: String): Int = solve2(text)

@main def hello: Unit = 
  def text = Source.fromFile(filename).mkString
  println(partOne(text))
  println(partTwo(text))
