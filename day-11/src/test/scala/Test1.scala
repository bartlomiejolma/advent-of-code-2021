import org.junit.Test
import org.junit.Assert.*

val input = """5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"""

class Test1:
  @Test def t1(): Unit = 
    assertEquals(1656, partOne(input))

  @Test def t2(): Unit = 
    assertEquals(195, partTwo(input))