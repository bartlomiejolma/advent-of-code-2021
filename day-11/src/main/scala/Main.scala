
import scala.io.Source

val filename = "input.txt"


type Point = (Int, Int)
type Octopuses = Map[Point, Int]


def visualizeOctopuses(octopuses: Octopuses): String = {
  val minX = octopuses.keys.map(_._1).min
  val maxX = octopuses.keys.map(_._1).max
  val minY = octopuses.keys.map(_._2).min
  val maxY = octopuses.keys.map(_._2).max

  val rows = (minY to maxY).toList.map { y =>
    (minX to maxX).toList.map { x =>
      octopuses.getOrElse((x, y), '.')
    }.mkString
  }.mkString("\n")

  rows
}

def parseInput(text: String): Octopuses = 
  val lines = text.split("\n")
  lines.zipWithIndex.map( (lineInput: ( String, Int)) =>
    val (line, y) = lineInput
    line.toCharArray.zipWithIndex map { case (c: Char, x) =>
      (x, y) -> c.toString.toInt
    }
  ).flatten.toMap


def getNeighborIndexes(x: Int, y: Int): List[Point] = 
  List(
     (x-1 ,y),
     (x-1 ,y-1),
     (x-1 ,y+1),

     (x+1 ,y),
     (x+1 ,y-1),
     (x+1 ,y+1),

     (x ,y-1),
     (x ,y+1),
  )

def considerFlashes(octopuses: Octopuses, prevFlashes: Int, stepFlashed: Octopuses): (Octopuses, Int) = {

  val nines = octopuses.filter(_._2 > 9)
  nines.size match {
    case 0 => (octopuses, prevFlashes)
    case _ => {
      val newOctopuses = nines.map { case (point, _) =>
        val neighbors = getNeighborIndexes(point._1, point._2)
        neighbors.map((_, 1))
      }
      val ninesZeroed = nines.map { case (point, _) =>
        (point, 0)
      }.toMap ++ stepFlashed
      val nonNines = octopuses -- ninesZeroed.keys
      val newOctopusesFlat = ((newOctopuses 
      .flatten ++ nonNines.toArray)
      .groupBy(_._1).mapValues(_.map(_._2).sum).toMap ++ ninesZeroed)
      .filterKeys(octopuses.contains).toMap
      considerFlashes(newOctopusesFlat, prevFlashes + nines.size, ninesZeroed)
    }
  }
}
def step(acc: (Octopuses, Int), idx: Int): (Octopuses, Int) = {

  val (octopuses, prevFlashes) = acc
  println(idx)
  println(visualizeOctopuses(octopuses))
  considerFlashes(octopuses.mapValues(c => c + 1).toMap, prevFlashes, Map[Point, Int]())
}
def partOne(text: String, steps: Int = 100): Int = {
  val octopuses = parseInput(text)
  val result = (1 to steps).foldLeft((octopuses, 0))(step)
  result._2
}

def partTwo(text: String, steps: Int = 300): Int = {
  var octopuses = parseInput(text)
  for (i <- 0 to steps) {
    octopuses.forall(_._2 == 0) match {
      case true => return i
      case _ => {
        octopuses = step((octopuses, 0), i)._1
      }
    }
  }
  0
}
  
@main def hello: Unit = 
  val text = Source.fromFile(filename).mkString
  println(partOne(text))
  println(partTwo(text))
