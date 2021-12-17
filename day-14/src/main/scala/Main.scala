import scala.io.Source

val filename = "input.txt"

def parseRule(lineText: String): (Sequence, Char) = 
  val elements::newOne = lineText.split(" -> ").toList
  
  ((elements(0).toChar, elements(1).toChar), newOne.head(0).toChar)


type Sequence = (Char, Char)
type Template = Map[Sequence, Long]
type Rules = Map[Sequence, Char]

def parseInput(input: String): (Template, Rules) = {
  val head::tail::rest = input.split("\n\n").toList
  val template = (head.toSeq.map(_.toChar).toArray
    .sliding(2)
    .map { case Array(f1,f2) => (f1,f2) }
    .toArray
    :+ (head.last, ' '))
    .groupBy(identity).mapValues(_.length.toLong).toMap
  val rules = tail.split("\n").map(parseRule)
  rules.foreach(println)
  (template , rules.toMap) 
}

def step(rules: Rules)(template: Template, idx: Int): Template = {
  val templateLength = template.size
  println(s"step $idx length: $templateLength")
  template.flatten { case (sequence, c) =>
    rules.get(sequence) match {
      case Some(d) => Array((sequence(0), d) -> c, (d, sequence(1)) -> c)
      case None => Array(sequence -> c)
    }
  }
  .groupMapReduce(x => x._1)(_._2)(_ + _)
  .toMap
}
def calculateResult(polymer: Template): Long =
  polymer.foreach(println)

  val counts = polymer.groupBy(_._1._1).mapValues(_.map(_._2).sum).values
  counts.max - counts.min


def partOne(text: String, counts: Int = 10): Long = 
  val (template, rules) = parseInput(text)
  template.foreach(println)
  val polymer = (1 to counts).foldLeft(template)(step(rules))
  calculateResult(polymer)


@main def hello: Unit = 
  def text = Source.fromFile(filename).mkString
  println(partOne(text))
  println(partOne(text, 40))


