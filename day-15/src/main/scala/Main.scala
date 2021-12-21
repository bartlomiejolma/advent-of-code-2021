import scala.io.Source

val filename = "input.txt"

type Point = (Int, Int)
type RiskLevels = Map[Point, Int]

def visualizeRiskLevels(riskLevels: RiskLevels): String = {
  val minX = riskLevels.keys.map(_._1).min
  val maxX = riskLevels.keys.map(_._1).max
  val minY = riskLevels.keys.map(_._2).min
  val maxY = riskLevels.keys.map(_._2).max

  val grid = (minX to maxX).map(x => (minY to maxY).map(y => riskLevels.getOrElse((x, y), 0)).mkString("")).mkString("\n")

  grid
}

def visualizeBestPaths(bestPaths: scala.collection.mutable.Map[Point, Int]): String = {
  val minX = bestPaths.keys.map(_._1).min
  val maxX = bestPaths.keys.map(_._1).max
  val minY = bestPaths.keys.map(_._2).min
  val maxY = bestPaths.keys.map(_._2).max

  val grid = (minX to maxX).map(x => (minY to maxY).map(y => bestPaths.getOrElse((x, y), 0)).mkString(" ")).mkString("\n")

  grid
}
def getNeighborIndexes(point: Point): List[Point] = {
  val (x, y) = point
  List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
}

def getNeighbors(point: Point, riskLevels: RiskLevels): List[Point] = {
  getNeighborIndexes(point).filter(p => riskLevels.contains(p))
}

def solve(riskLevels: RiskLevels): Int = 
  val maxY = riskLevels.keys.map(_._2).max
  val maxX = riskLevels.keys.map(_._1).max

  val bestPaths = scala.collection.mutable.Map[Point, Int]()
  val endPoint = (maxX, maxY)
  var candidates = List[Point](endPoint)

  bestPaths((maxX, maxY)) = 0
  val visited = scala.collection.mutable.Set[Point]()

  while (candidates.nonEmpty) {
    val current = candidates.head
    val currentRisk = bestPaths(current) + riskLevels(current)
    val neighbors = getNeighbors(current, riskLevels)
    val newCandidates = neighbors
      .map(n => 
        val existing = bestPaths.getOrElse(n, 10000)
        currentRisk < existing match {
          case true => 
            bestPaths(n) = currentRisk
            visited -= n
          case false => None
        }
        n
      )
      .filter(n => !visited(n))
    visited += current
    candidates = (candidates.tail ++ newCandidates).distinct
  }

    println(visualizeBestPaths(bestPaths))

  bestPaths((0, 0))

def parseInput(text: String): RiskLevels = 
  text.split("\n").zipWithIndex.flatMap { case (line, y) =>
    line.split("").zipWithIndex.map { case (char, x) =>
      (y, x) -> char.toInt
    }
  }.toMap

def expandTile(riskLevels: RiskLevels, x_tile: Int, y_tile: Int): RiskLevels = {
  val maxY = 1 + riskLevels.keys.map(_._2).max
  val maxX = 1 + riskLevels.keys.map(_._1).max
  riskLevels.map { case (point, risk) =>
    val newRisk = risk + x_tile + y_tile 
    val newPoint = (point._1 + x_tile * maxX , point._2 + y_tile * maxY)
    (newPoint, if (newRisk >= 10) newRisk - 9 else newRisk)
  }
}
def expand(riskLevels: RiskLevels, times: Int = 5): RiskLevels = {
  (0 to times - 1).map(
    x => (0 to times - 1).map(y => expandTile(riskLevels, x, y)).flatten.toMap).flatten.toMap
}
def partOne(text: String): Int = {
  val riskLevels = parseInput(text)

  println(visualizeRiskLevels(riskLevels))

  solve(riskLevels)
}

def partTwo(text: String): Int = {
  val riskLevels = parseInput(text)
  val expandedRiskLevels = expand(riskLevels, 5)
  println(visualizeRiskLevels(expandedRiskLevels))

  solve(expandedRiskLevels)
}

@main def hello: Unit = 
  val text = Source.fromFile(filename).mkString
  println(partOne(text))
  println(partTwo(text))
