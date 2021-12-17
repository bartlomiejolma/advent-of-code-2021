import org.junit.Test
import org.junit.Assert.*

class Test1:
  val boundaries = Boundaries(20, 30, -10, -5)
  val x0 = 0
  val y0 = 0

  @Test def tRun(): Unit = 
    val start = State(x0, y0, 7, 2)
    assertEquals(Some(3), run(boundaries)(start, 0))
  @Test def tRun2(): Unit = 
    val start = State(x0, y0, 9, 0)
    assertEquals(Some(0), run(boundaries)(start, 0))

  @Test def tRun3(): Unit = 
    val start = State(x0, y0, 6, 3)
    assertEquals(Some(6), run(boundaries)(start, 0))

  @Test def tRunNone(): Unit = 
    val start = State(x0, y0, 17, -4)
    assertEquals(None, run(boundaries)(start, 0))

  @Test def tRunMax(): Unit = 
    val start = State(x0, y0, 6, 9)
    assertEquals(Some(45), run(boundaries)(start, 0))
  
  @Test def tRunBruteForce(): Unit = 
    assertEquals(45, bruteForce(boundaries))