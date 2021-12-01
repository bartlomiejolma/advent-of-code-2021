import scala.io.Source

val filename = "input.txt"

@main def hello: Unit = 
  def text = Source.fromFile(filename).getLines
  def increaseCount = text.sliding(2).count(pair => pair(1).toInt > pair(0).toInt) 
  println(increaseCount)

