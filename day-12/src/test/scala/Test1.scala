import org.junit.Test
import org.junit.Assert.*


val input = """start-A
start-b
A-c
A-b
b-d
A-end
b-end"""


class Test1:
  @Test def t1(): Unit = 
    assertEquals(10, partOne(input))

  @Test def t2(): Unit = 
    assertEquals(36, partTwo(input))