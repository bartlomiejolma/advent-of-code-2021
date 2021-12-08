import scala.io.Source

val filename = "input.txt"


def parseInput(input: String): scala.collection.mutable.Map[Int, Long] = {
  scala.collection.mutable.Map[Int, Long](input.split(",").map(_.toInt).groupBy(identity).map(t => (t._1, (t._2.length).toLong)).toSeq: _*)
}


def generation(counts: scala.collection.mutable.Map[Int, Long], count: Int): scala.collection.mutable.Map[Int, Long] = {
  val newCounts = scala.collection.mutable.Map[Int, Long]()
  counts foreach { case (k, v) => k match {
    case 0 => { 
      newCounts.get(6) match {
        case Some(x) => newCounts += (6 -> (x + v))
        case None => newCounts += (6 -> v)
      }
      newCounts += (8 -> v)
    }
    case _ => newCounts.get(k - 1) match {
        case Some(x) => newCounts += (k - 1 -> (x + v))
        case None => newCounts += (k - 1 -> v)
      }
  }}
  newCounts
}


def solve(text: String, generations: Int): Long = {
  val counts = parseInput(text)
  val result = (1 to generations).foldLeft(counts)(generation)
  result.values.sum
}
def partOne(text: String): Long = solve(text, 80)
def partTwo(text: String): Long = solve(text, 256)

@main def hello: Unit = 
  def text = Source.fromFile(filename).mkString
  println(partOne(text))
  println(partTwo(text))
