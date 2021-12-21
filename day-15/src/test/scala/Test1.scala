import org.junit.Test
import org.junit.Assert.*

val input = """1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"""

class Test1:
  @Test def t1(): Unit = 
    assertEquals(40, partOne(input))
