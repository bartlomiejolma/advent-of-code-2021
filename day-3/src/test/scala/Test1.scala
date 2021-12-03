import org.junit.Test
import org.junit.Assert.*


val sampleInput = """00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010""".split("\n").toList

class Test1:
  @Test def t1(): Unit = 
    assertEquals((22, 9), decode(sampleInput))


class Test2:
  @Test def t1(): Unit = 
    assertEquals((23, 10), decode2(sampleInput))

