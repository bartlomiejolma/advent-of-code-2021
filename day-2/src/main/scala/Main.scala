import scala.io.Source

val filename = "input.txt"

def move1(text: List[String]): (Int, Int) = 
  def initialPosition = (0,0)

  val position = text.map(
    instruction => instruction match {
      case s"forward $distance" => (distance.toInt, 0)
      case s"up $distance" => (0, -distance.toInt)
      case s"down $distance" => (0, distance.toInt)
    }
  ).fold(initialPosition)((acc, x) => (acc._1 + x._1, acc._2 + x._2))
  println(position)
  position 


def partOne: Unit = 
  def text = Source.fromFile(filename).getLines.toList
  val (horizontal, vertical) = move1(text)
  println(horizontal * vertical)


@main def hello: Unit = 
  partOne
