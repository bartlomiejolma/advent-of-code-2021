import scala.io.Source

val filename = "input.txt"

def parseRule(lineText: String): ((Char, Char), Char) = 
  val elements::newOne = lineText.split(" -> ").toList
  
  ((elements(0).toChar, elements(1).toChar), newOne.head(0).toChar)



def parseInput(input: String): (Array[Char], Map[(Char, Char), Char]) = {
  val head::tail::rest = input.split("\n\n").toList
  val template = head.toSeq.map(_.toChar).toArray
  val rules = tail.split("\n").map(parseRule)
  rules.foreach(println)
  (template , rules.toMap) 
}

def step(rules: Map[(Char, Char), Char])(template: Array[Char], idx: Int): Array[Char] = {
  template.
  sliding(2).map(s =>
      rules.get((s(0), s(1))) match {
        case Some(c) => Array[Char](s(0), c)
        case None => Array[Char](s(0))
      }).flatten.toArray :+ template.last
}

def calculateResult(polymer: Array[Char]): Long =
  val counts = polymer.groupBy(identity).mapValues(_.length).map(_._2)
  counts.max - counts.min

def partOne(text: String, counts: Int = 10): Long = 

  val (template, rules) = parseInput(text)
  val polymer = (1 to counts).foldLeft(template)(step(rules))
  println(polymer.mkString)
  calculateResult(polymer)


@main def hello: Unit = 
  def text = Source.fromFile(filename).mkString
  println(partOne(text))
  println(partOne(text, 40))


