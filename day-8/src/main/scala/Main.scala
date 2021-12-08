
import scala.io.Source

val filename = "input.txt"

type Digit = String
type OutputValue = Tuple4[Digit, Digit, Digit, Digit]
type UniqueSignalPattern = Array[Digit]
type Line = Tuple2[UniqueSignalPattern, OutputValue]


def parseLine(lineRepr: String): Line = {
  val lineInput = lineRepr.split(" \\| ").map(x => x.split(' ')).toList
  return (lineInput(0), (lineInput(1)(0), lineInput(1)(1), lineInput(1)(2), lineInput(1)(3)))
}


def parseInput(input: String): List[Line] = {
  return input.split("\n").map(parseLine).toList
}



val uniqueLengths = Array[Int](2, 3, 4, 7)
def partOne(text: String): Int = {

    val lines = parseInput(text)
    lines.map( line => {
      val (pattern, output) = line
      output.productIterator
      .map(_.asInstanceOf[Digit])
      .map(digit => digit.length)
    })
    .flatten
    .groupBy(identity)
    .filterKeys(uniqueLengths.contains(_))
    .mapValues(_.size)
    .values
    .sum
}



@main def hello: Unit = 
  def text = Source.fromFile(filename).mkString
  println(partOne(text))
