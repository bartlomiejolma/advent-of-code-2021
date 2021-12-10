
import scala.io.Source

val filename = "input.txt"

type Digit = Set[Char]
type OutputValue = Tuple4[Digit, Digit, Digit, Digit]
type UniqueSignalPattern = Array[Digit]
type Line = Tuple2[UniqueSignalPattern, OutputValue]

type DecodingMap = Map[Char, Char]

def parseLine(lineRepr: String): Line = {
  val lineInput = lineRepr.split(" \\| ")
    .map(
      x => x
          .split(' ')
          .map(x => Set[Char](x.toSeq:_*))
      )
    .toList

  return (lineInput(0), (lineInput(1)(0), lineInput(1)(1), lineInput(1)(2), lineInput(1)(3)))
}

def parseInput(input: String): List[Line] = {
  return input.split("\n").map(parseLine).toList
}


def decodeLine(line: Line): Int = {


  val pattern = line._1
  val output = line._2
  val mapping = decodePattern(pattern)
  decodeOutput(output, mapping)
}


def parseDecode(line: String): Int = decodeLine(parseLine(line))
val digits = Map[Int, Digit](
  0 -> Set('a', 'b', 'c', 'e', 'f', 'g'),
  1 -> Set('c', 'f'),
  2 -> Set('a', 'c', 'd', 'e', 'g'),
  3 -> Set('a', 'c', 'd', 'f', 'g'),
  4 -> Set('b', 'c', 'd', 'f'),
  5 -> Set('a', 'b', 'd', 'f', 'g'),
  6 -> Set('a', 'b', 'e', 'd', 'f', 'g'),
  7 -> Set('a', 'c', 'f'),
  8 -> Set('a', 'b', 'c', 'd', 'e', 'f', 'g'),
  9 -> Set('a', 'b', 'c', 'd', 'f', 'g')
)

type Lengths = Map[Int, Array[(Digit, Int)]]
def find(number: Int)(lengths:Lengths): Array[Digit] = lengths(digits(number).size).map(_._1)

def findOne (number: Int)(lengths: Lengths): Digit = find(number)(lengths).head

def finda(one: Digit, seven: Digit): Char = (seven &~ one).head


def findc(eight: Digit, six: Digit): Char = (eight &~ six).head

def findf(one: Digit, c: Char): Char = (one - c).head


def find2(f: Char, twofivethree: Array[Digit]): Digit = (twofivethree.find(x => !x(f) ).head)
def find5(c: Char, twofivethree: Array[Digit]): Digit = (twofivethree.find(x => !x(c) ).head)

def finde(six: Digit, five: Digit): Char = (six &~ five).head


def find9(sixninezero: Array[Digit], e: Char): Digit = sixninezero.find(x => !x(e)).head

def getLengths(pattern: UniqueSignalPattern): Lengths = pattern.map(x => (x, x.size)).groupBy(_._2)

def find6(sixninezero: Array[Digit], one: Digit): Digit = sixninezero.filter(x => !(one subsetOf x)).head

def find0(sixninezero: Array[Digit], six: Digit, nine: Digit): Digit = sixninezero.filter(x => x != six && x != nine).head

def findd(eight: Digit, zero: Digit): Char = (eight &~ zero).head

def findb(four: Digit, d: Char, one: Digit): Char = ((four &~ one) - d).head



def findg(eight: Digit, four: Digit, a: Char, e: Char): Char = ((eight &~ four) - a - e).head



def decodePattern(pattern: UniqueSignalPattern): DecodingMap = {
  val lengths = getLengths(pattern)
  val seven = findOne(7)(lengths)
  val one = findOne(1)(lengths)

  val eight = findOne(8)(lengths)
  val four = findOne(4)(lengths)

  val sixninezero = find(6)(lengths)
  val twofivethree = find(2)(lengths)

  val a = finda(one, seven)
  val six = find6(sixninezero, one)

  val c = findc(eight, six)

  val f = findf(one, c)
  val five = find5(c, twofivethree)

  val e = finde(six, five)

  val nine = find9(sixninezero, e)
  val zero = find0(sixninezero, six, nine)

  val d = findd(eight, zero)
  val b = findb(four, d, one)
  val g = findg(eight, four, a, e)
  return Map[Char, Char](
    a -> 'a',
    b -> 'b',
    c -> 'c',
    d -> 'd',
    e -> 'e',
    f -> 'f',
    g -> 'g',
  )
}

def decodeOutputDigit(digit: Digit, decodingMap: DecodingMap): Int = {
  val correctDigits = digit.map(x => decodingMap(x))
  digits.filter(_._2 == correctDigits).keys.head
}

def decodeOutput(output: OutputValue, decodingMap: Map[Char, Char]): Int = {

  val a = decodeOutputDigit(output._1, decodingMap)
  val b = decodeOutputDigit(output._2, decodingMap)
  val c = decodeOutputDigit(output._3, decodingMap)
  val d = decodeOutputDigit(output._4, decodingMap)

  return a * 1000 + b * 100 + c * 10 + d
}

val uniqueLengths = Array[Int](2, 3, 4, 7)
def partOne(text: String): Int = {

    val lines = parseInput(text)
    lines.map( line => {
      val (pattern, output) = line
      output.productIterator
      .map(_.asInstanceOf[Digit])
      .map(digit => digit.size)
    })
    .flatten
    .groupBy(identity)
    .filterKeys(uniqueLengths.contains(_))
    .mapValues(_.size)
    .values
    .sum
}


def partTwo(text: String): Int = {
    val lines = parseInput(text)
    lines.map(decodeLine).sum
}


@main def hello: Unit = 
  def text = Source.fromFile(filename).mkString
  println(partOne(text))
  println(partTwo(text))
