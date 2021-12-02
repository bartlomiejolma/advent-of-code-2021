import org.junit.Test
import org.junit.Assert.*


val sampleInput = """forward 5
down 5
forward 8
up 3
down 8
forward 2""".split("\n").toList

class Test1:
  @Test def t1(): Unit = 
    def text = List("forward 1", "up 1", "forward 1")
    assertEquals((2, -1), move1(text))

  @Test def tsample(): Unit = 
    def text = sampleInput
    assertEquals((15, 10), move1(text))



class Test2:
  @Test def t1(): Unit = 
    def text = List("forward 1", "up 1", "forward 1")
    val (horizontal, vertical, _ ) = move2(text)
    assertEquals(2, horizontal)
    assertEquals(-1, vertical)

  @Test def tsample(): Unit = 
    def text = sampleInput
    val (horizontal, vertical, _ ) = move2(text)
    assertEquals(15, horizontal)
    assertEquals(60, vertical)