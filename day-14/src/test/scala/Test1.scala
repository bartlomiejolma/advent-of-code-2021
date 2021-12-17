import org.junit.Test
import  org.junit.Assert.*

val input = """NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"""

class Test1:
  @Test def t1(): Unit = 
    assertEquals(1588, partOne(input))

  @Test def t2(): Unit = 
    assertEquals(2188189693529L, partOne(input, 40))