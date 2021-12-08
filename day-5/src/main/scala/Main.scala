import scala.io.Source

val filename = "input.txt"

case class Line(x1: Int, y1: Int, x2: Int, y2: Int)

def isVertical(line: Line): Boolean = line.x1 == line.x2
def isHorizontal(line: Line): Boolean = line.y1 == line.y2
def isDiagonal(line: Line): Boolean = !isVertical(line)  && !isHorizontal(line) && (line.x1 - line.x2).abs == (line.y1 - line.y2).abs
def shouldBeConsidered1(line: Line): Boolean = isVertical(line) || isHorizontal(line) 
def shouldBeConsidered2(line: Line): Boolean = isVertical(line) || isHorizontal(line) || isDiagonal(line)


def parseLine(lineRepr: String): Line = {
  val lineInput = lineRepr.split(" -> ").map(x => x.split(",").map(_.toInt)).flatten
  return Line(lineInput(0), lineInput(1), lineInput(2), lineInput(3))
}


def parseInput(input: String): List[Line] = {
  return input.split("\n").map(parseLine).toList
}

def handleElement(elements: scala.collection.mutable.Map[(Int, Int), Int], x: Int, y: Int): Unit = {
  if (elements.contains((x, y))) {
    elements((x, y)) = elements((x, y)) + 1
  } else {
    elements((x, y)) = 1
  }
}

def solve(shouldBeConsidered: (Line) => Boolean)(text: String): Int = {
  val lines = parseInput(text)
  val elements = scala.collection.mutable.Map[(Int, Int), Int]()
  lines.map(line => {
    shouldBeConsidered(line) match  {
      case true => {

        var stepx = if (line.x1 < line.x2) 1 else -1
        var stepy = if (line.y1 < line.y2) 1 else -1
        val len = List((line.x1 - line.x2).abs, (line.y1 - line.y2).abs).max
        var it = 0 to len
        var x = line.x1
        var y = line.y1
        it.foreach(iterator => {
          handleElement(elements, x, y)
          stepx = if (x != line.x2) stepx  else 0
          stepy = if (y != line.y2) stepy  else 0
          x += stepx
          y += stepy
        })
      }
      case _ => None
    }
    }
  )
  return elements.count(_._2 > 1)
}
def partOne(text: String): Int = solve(shouldBeConsidered1)(text)

def partTwo(text: String): Int = solve(shouldBeConsidered2)(text)


@main def hello: Unit = 
  def text = Source.fromFile(filename).mkString
  println(partOne(text))
  println(partTwo(text))
