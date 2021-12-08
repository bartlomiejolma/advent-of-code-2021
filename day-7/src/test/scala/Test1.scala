import org.junit.Test
import org.junit.Assert.*

val input = """16,1,2,0,4,2,7,1,2,14"""

class Test1:
  @Test def t1(): Unit = 
    assertEquals(37, partOne(input))

  @Test def t2(): Unit = 
    assertEquals(168, partTwo(input))

  @Test def tcosts(): Unit = 
    assertEquals(168, costs(Array[Int](16, 1, 2, 0, 4, 2, 7, 1, 2, 14), 5))
    assertEquals(206, costs(Array[Int](16, 1, 2, 0, 4, 2, 7, 1, 2, 14), 2))
