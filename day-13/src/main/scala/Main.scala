
import scala.io.Source

val filename = "input.txt"


type Point = (Int, Int)
type Paper = Set[Point]
type Instruction = (Int, Int)
type Instructions = Array[Instruction]

def parseInstruction(line: String): Instruction = {
  line match {
    case s"fold along y=$y" => (0, y.toInt)
    case s"fold along x=$x" => (x.toInt, 0)
  }
}

def parsePointLine(line: String): Point = {
  val Array(x, y) = line.split(",").map(_.toInt)
  (x, y)
}

def visualizePaper(paper: Paper): String = 
  val minX = paper.map(_._1).min
  val maxX = paper.map(_._1).max
  val minY = paper.map(_._2).min
  val maxY = paper.map(_._2).max

  val rows = (minY to maxY).toList.map { y =>
    (minX to maxX).toList.map { x =>
      paper contains (x, y) match {
        case true => "#"
        case false => "."
      }
    }.mkString
  }.mkString("\n")

  rows


def parseInput(text: String): (Paper, Instructions) = 
  val paperInput::instructionInput::_ = text.split("\n\n").toList
  val paper = paperInput.split("\n").map(parsePointLine).toSet
  val instructions = instructionInput.split("\n").map(parseInstruction)
  (paper, instructions)

def valueIfFold(cor: Int, foldCor:Int): Int = 
    if (cor > foldCor) foldCor -  (cor - foldCor) else  cor
def fold(paper: Paper, instruction: Instruction) : Paper = 
  instruction match {
    case (0, yFold) => paper.map { case (x, y) => (x, valueIfFold(y, yFold)) }
    case (xFold, 0) => paper.map { case (x, y) => (valueIfFold(x, xFold), y) }
  }
def partOne(text: String): Int = {
  val (paper, instructions) = parseInput(text)
  val foldedPaper = fold(paper, instructions.head)

  foldedPaper.size
}

def partTwo(text: String): Int = {
  val (paper, instructions) = parseInput(text)
  val foldedPaper = instructions.foldLeft(paper)(fold)
  println(visualizePaper(foldedPaper))
  foldedPaper.size
}
  
@main def hello: Unit = 
  val text = Source.fromFile(filename).mkString
  println(partOne(text))
  println(partTwo(text))
