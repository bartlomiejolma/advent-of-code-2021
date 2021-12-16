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

val autocompleteScores = Map[Char, Int](
  ')' -> 1,
  ']' -> 2,
  '}' -> 3,
  '>' -> 4
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

def autocompleteLine(expected: List[Char])(line: String): List[Char] = 

  // println(s"autocompleteLine($expected)($line)")
  line match {
    case "" => expected
    case _ => {

      expected match {
        case e::rest if e == line.head => autocompleteLine(rest)(line.tail)
        case _ =>  autocompleteLine(expectedNext(line.head) :: expected)(line.tail)
        }
      }
    }


def partOne(text: String): Int = {

    text.split('\n').map(x => parseLine(None)(x)._1).flatten.map(scores).sum
}


def partTwo(text: String): Long = {

    val lines = text.split('\n')
    val scores =
    lines.map(x => parseLine(None)(x))
    .zip(lines)
    .filter(x => !x._1._1.isDefined).map(_._2)
    .map(autocompleteLine(List[Char]()))
    .map(
      x => x.map(autocompleteScores)
            .foldLeft(0L)((acc, score) => acc * 5 + score)
    )
    .sortWith(_ > _)
    scores(scores.length  / 2)
}
@main def hello: Unit = 
  val text = Source.fromFile(filename).mkString
  println(partOne(text))
  println(partTwo(text))

