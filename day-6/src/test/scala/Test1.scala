import org.junit.Test
import org.junit.Assert.*

val input = """3,4,3,1,2"""

class Test1:
  @Test def t1(): Unit = 
    assertEquals(26, solve(input, 18))
    assertEquals(5934, solve(input, 80))
    assertEquals(26984457539L, solve(input, 256))
