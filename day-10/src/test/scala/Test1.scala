import org.junit.Test
import org.junit.Assert.*


val input = """[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"""


class Test1:
  @Test def t1(): Unit = 
    assertEquals(26397, partOne(input))
 
  @Test def autocomplete(): Unit = 
    assertEquals("}}]])})]", autocompleteLine(List[Char]())("[({(<(())[]>[[{[]{<()<>>").mkString)
  @Test def t2(): Unit = 
    assertEquals(288957, partTwo(input))