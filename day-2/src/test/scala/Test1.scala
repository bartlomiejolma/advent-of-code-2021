import org.junit.Test
import org.junit.Assert.*

class Test1:
  @Test def t1(): Unit = 
    def text = List("forward 1", "up 1", "forward 1")
    assertEquals((2, -1), move1(text))

  @Test def tsample(): Unit = 
    def text = """forward 5
down 5
forward 8
up 3
down 8
forward 2"""
    assertEquals((15, 10), move1(text.split("\n").toList))

