
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

@main def hello: Unit = 
  val text = Source.fromFile(filename).mkString
  println(partOne(text))
