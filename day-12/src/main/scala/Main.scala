import scala.io.Source

val filename = "input.txt"

def parseLine(line: String): (String, String) = {
  val parts = line.split("-")
  (parts(0), parts(1))
}

type Nodes = Map[String, Array[String]]
def parseInput(text: String): Nodes = 
  val lines = text.split("\n")
  lines.map(parseLine)
  .map(t => List((t._1, t._2),(t._2, t._1)))
  .flatten
  .groupBy(_._1)
  .map((kv) => (kv._1, kv._2.map(_._2)))
  .toMap


def findStart(nodes: Nodes): String = {
  val next = nodes.map(_._2).flatten.toSet
  val all = nodes.keys.toSet
  (all -- next).head
}


def search(nodes:Nodes, way: List[String], current: String): Array[List[String]] =
  println(way.mkString(" <- "))

  println(current)
  current match {
    case "end" => Array(current :: way)
    case s"$next" => nodes(current)
      .filter(a_next => !way.contains(a_next) || a_next == a_next.toUpperCase)
      .flatMap(a_next => search(nodes, current :: way, a_next))
  }


def partOne(text: String): Int = {
  val ways = parseInput(text)
  ways.foreach(kv => println(kv._1 + " -> " + kv._2.mkString(", ")))
  search(ways, List(), "start").length
}

def search2(nodes:Nodes, way: List[String], current: String, usedRevisit: Boolean): Array[List[String]] =
  current match {
    case "end" => Array(current :: way)
    case s"$next" => nodes(current)
      // .filter(a_next => !way.contains(a_next) || a_next == a_next.toUpperCase)
      .flatMap(a_next => 
        if (way.contains(a_next) && a_next != a_next.toUpperCase) {
          if (usedRevisit || a_next == "start") {
            Array[List[String]]()
          } else {
            search2(nodes, current :: way, a_next, true)
          }
        } else {
          search2(nodes, current :: way, a_next, usedRevisit)
        }
      )
  }

def partTwo(text: String): Int = {
  val ways = parseInput(text)
  ways.foreach(kv => println(kv._1 + " -> " + kv._2.mkString(", ")))
  search2(ways, List(), "start", false).length
}

@main def hello: Unit = 
  val text = Source.fromFile(filename).mkString
  println(partOne(text))
  println(partTwo(text))