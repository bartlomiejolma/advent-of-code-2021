import org.junit.Test
import org.junit.Assert.*

class Test1:
  @Test def t1(): Unit = 
    val playerStartState = Map[String, (Int, Int)](
      "1" -> (4, 0),
      "2" -> (8, 0)
    )

    assertEquals(739785, partOne(playerStartState))
