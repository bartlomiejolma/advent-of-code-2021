import org.junit.Test
import org.junit.Assert.*


val input = """2199943210
3987894921
9856789892
8767896789
9899965678"""


class Test1:
  @Test def t1(): Unit = 
    assertEquals(15, partOne(input))
