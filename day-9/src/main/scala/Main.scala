
import scala.io.Source

val filename = "input.txt"


case class HeightMap(data: Array[Array[Int]]) {
  def get(x: Int)(y: Int): Option[Int] = data.lift(x) match {
    case Some(row) => row.lift(y)
    case None => None
  }
}


def parseInput(text: String): HeightMap = 
  val lines = text.split("\n")
  HeightMap(lines.map(line => line.split("").map(_.toInt)))


def getNeighbors(heightMap: HeightMap, x: Int, y: Int): List[Int] = 
  List(
    heightMap.get(x-1)(y),
    heightMap.get(x+1)(y),
    heightMap.get(x)(y+1),
    heightMap.get(x)(y-1)
  ).flatten

def partOne(text: String): Int = {
  val heightMap = parseInput(text)
  val lowpoints = heightMap.data.zipWithIndex
  .map { case (row, x) => 
      row.zipWithIndex.map { case (height, y) =>  {
          val neighbors = getNeighbors(heightMap, x, y)
          if (neighbors.forall(neighborHeight => neighborHeight > height)) height + 1 else 0
        }
      }
    }
  lowpoints.flatten.sum
}

type Visited = scala.collection.mutable.Map[(Int, Int), (Int, Int)]

def go(heightMap:HeightMap)(start: (Int, Int))(visited: Visited, x: Int, y: Int): Unit = 

  val height = heightMap.get(x)(y)
  // println(s"$x, $y $height $start $visited")
    visited.get((x,y)) match {
      case None =>
        height match {
          case Some(9) => visited((x,y)) = (-1, -1)
          case None => visited((x,y)) = (-1, -1)
          case Some(validHeight) => 
            visited((x,y)) = start
            go(heightMap)(start)(visited, x+1, y)
            go(heightMap)(start)(visited, x-1, y)
            go(heightMap)(start)(visited, x, y - 1)
            go(heightMap)(start)(visited, x, y + 1)
        }
      case Some(start) =>
    }
  
def partTwo(text: String): Int = 
  val heightMap = parseInput(text)
  val visited = scala.collection.mutable.Map[(Int, Int), (Int, Int)]()
  val lowpoints = heightMap.data.zipWithIndex
  .foreach { case (row, x) => 
      row.zipWithIndex.map { case (height, y) =>  
        go(heightMap)((x,y))(visited, x, y)
      }
    }

  val basins = visited.filter(e => e._2._1 != -1).groupBy(_._2)
  println(basins)

  basins.map(e => e._2.size).toArray.sortWith(_ > _).zip(1 to 3).map(_._1).reduce(_ * _)

  
@main def hello: Unit = 
  val text = Source.fromFile(filename).mkString
  println(partOne(text))
  println(partTwo(text))
