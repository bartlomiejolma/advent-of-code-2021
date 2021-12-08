import org.junit.Test
import org.junit.Assert.*

val input = """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"""

class Test1:
  @Test def t1(): Unit = 
    assertEquals(5, partOne(input))

  @Test def t2(): Unit = 
    assertEquals(12, partTwo(input))
