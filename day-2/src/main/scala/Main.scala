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
  // println(position)
  position 


def move2(text: List[String]): (Int, Int, Int) = 
  val initialPosition = (0,0,0)

  val position = text.map(
    instruction => instruction match {
      case s"forward $distance" => (distance.toInt, 0)
      case s"up $distance" => (0, -distance.toInt)
      case s"down $distance" => (0, distance.toInt)
    }
  ).foldLeft(initialPosition)((current, command) => (current._1 + command._1, current._2 + command._1 * current._3, current._3 + command._2))
  println(position)
  position 

def partOne: Unit = 
  def text = Source.fromFile(filename).getLines.toList
  val (horizontal, vertical) = move1(text)
  println(horizontal * vertical)

def partTwo: Unit = 
  def text = Source.fromFile(filename).getLines.toList
  val (horizontal, vertical, _) = move2(text)
  println(horizontal * vertical)

@main def hello: Unit = 
  partOne
  partTwo
