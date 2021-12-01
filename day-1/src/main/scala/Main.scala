import scala.io.Source

val filename = "input.txt"

def partOne: Unit = 
  def text = Source.fromFile(filename).getLines
  def increaseCount = text.sliding(2).count(pair => pair(1).toInt > pair(0).toInt) 
  println(increaseCount)

def partTwo: Unit = 
  def text = Source.fromFile(filename).getLines
  def size = 3
  def averages = text.sliding(size).map(subset => subset.map(_.toInt).sum) 
  def increaseCount = averages.sliding(2).count(pair => pair(1) > pair(0)) 

  println(increaseCount)

@main def hello: Unit = 
  partOne
  partTwo
