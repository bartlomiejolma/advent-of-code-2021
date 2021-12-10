import org.junit.Test
import org.junit.Assert.*

val input = """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"""

class Test1:
  @Test def t1(): Unit = 
    assertEquals(26, partOne(input))


  val encoding = Map[Char, Char]('d' -> 'a', 'e' -> 'b', 'a' -> 'c', 'f' -> 'd', 'g' -> 'e', 'b' -> 'f', 'c' -> 'g')

  @Test def tDecodePattern(): Unit =

    val pattern = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab".split(' ').map(x => Set[Char](x.toSeq:_*)).toArray

    val lengths = getLengths(pattern)
    val seven = findOne(7)(lengths)
    val one = findOne(1)(lengths)

    val eight = findOne(8)(lengths)
    val four = findOne(4)(lengths)

    val sixninezero = find(6)(lengths)
    val twofivethree = find(2)(lengths)

    val a = finda(one, seven)
    val six = find6(sixninezero, four)

    val c = findc(eight, six)

    val f = findf(one, c)
    val five = find5(c, twofivethree)

    val e = finde(six, five)

    val nine = find9(sixninezero, e)
    val zero = find0(sixninezero, six, nine)

    val d = findd(eight, zero)
    val b = findb(four, d, one)
    val g = findg(eight, four, a, e)

    assertEquals(one, Set[Char]('a', 'b'))
    assertEquals(seven, Set[Char]('a', 'b', 'd'))
    assertEquals(eight, Set[Char]('a', 'b', 'c', 'd', 'e', 'f', 'g'))
    assertEquals(four, Set[Char]('a', 'b',  'e', 'f'))

    assertEquals(a, 'd')
    assertEquals(e, 'g')

    assertEquals(nine, Set[Char]('a', 'b', 'c', 'd', 'e', 'f'))

    assertEquals(six, Set[Char]('f', 'b', 'c', 'd', 'e', 'g'))

    assertEquals(zero, Set[Char]('a', 'b', 'c', 'd', 'e', 'g'))

    assertEquals(d, 'f')

    assertEquals(b, 'e')

    assertEquals(c, 'a')

    assertEquals(f, 'b')

    assertEquals(g, 'c')


    assertEquals(
      encoding, 
      decodePattern(pattern)
      )

  @Test def tDecodeOutput(): Unit =

    assertEquals(5, decodeOutputDigit(Set[Char]("cdfeb".toSeq:_*),encoding))
    assertEquals(3, decodeOutputDigit(Set[Char]("fcadb".toSeq:_*),encoding))
    assertEquals(3, decodeOutputDigit(Set[Char]("cdbaf".toSeq:_*),encoding))


  @Test def tDecodeOutputDigits(): Unit =

    assertEquals(5353, 
    decodeOutput(
      (
        Set[Char]("cdfeb".toSeq:_*),
        Set[Char]("fcadb".toSeq:_*),
        Set[Char]("cdfeb".toSeq:_*),
        Set[Char]("cdbaf".toSeq:_*)
      ),
      encoding
    ))
  @Test def tlineFull(): Unit =

    assertEquals(5353, parseDecode("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))

  @Test def tlineFullIssue(): Unit =
    assertEquals(9361, parseDecode("fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"))

  @Test def t2(): Unit =

    assertEquals(61229, partTwo(input))
