import scala.io.Source

val filename = "input.txt"

val expectedNext = Map[Char, Char](
  '(' -> ')',
  '[' -> ']',
  '{' -> '}',
  '<' -> '>'
)
val scores = Map[Char, Int](
  ')' -> 3,
  ']' -> 57,
  '}' -> 1197,
  '>' -> 25137
)
def parseLine(expected: Option[Char] = None)(line: String): (Option[Char], String) = 

  line match {
    case "" => (None, line)
    case _ => {
      val current = line.head
      expected match {
        case Some(e) if Some(e) == Some(current) => (None, line.tail)
        case Some(e) if expectedNext.values.toList.contains(current) => (Some(current), line.tail)
        case _ =>  { 
          parseLine(Some(expectedNext(current)))(line.tail) match {
            case (None, rest) => parseLine(expected)(rest)
            case (Some(e), rest) => (Some(e), rest)
          }
          }
        }
      }
    }

def partOne(text: String): Int = {

    text.split('\n').map(x => parseLine(None)(x)._1).flatten.map(scores).sum
}


@main def hello: Unit = 
  val text = Source.fromFile(filename).mkString
  println(partOne(text))

