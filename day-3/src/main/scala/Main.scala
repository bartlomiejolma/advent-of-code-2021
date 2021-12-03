import scala.io.Source

val filename = "input.txt"

def decode (text: List[String]): (Int, Int) =

  def binaryParse = Integer.parseInt(_, 2) 
  // Integer.bitCount()
  // // def xor = (a: Int, b: Int) => a ^ b
  val init = Vector.fill(text.head.length)(0)
  def linesCount = text.length / 2
  println(linesCount)
  // println(init)

  // val gamma = text
  //   .foldLeft(init)((counts, line) => 
  //       counts.zip(line.map(binaryParse))
  //       // .foreach(println)
  //       .map(x => { 
  //         println (x._1) 
  //         println (x._2) 
  //         println (line) 
  //         println (counts) 

  //         (x._1 +  x._2)}))
  //   // .map(x => x > linesCount)

  // val gamma = text
  //   .foldLeft(init)((a, b) => a.zip(b).map(x => x._1 + x._2))
  //   .map(binaryParse)
  //   .map(Integer.bitCount)
  //   .map(x => x > linesCount)
    
  // val gammaVal = binaryParse(gamma.map(x => if (x) "1" else "0")
  //   .mkString)
  // val epsilon = binaryParse(gamma.map(x => if (x) "0" else "1")
  //   .mkString)
  // println(gamma)

    val gamma = text
    .foldLeft(init)((a, b) => a.zip(b.map(x => if (x == '1') 1 else 0)).map(x => x._1 + x._2))
    // .map(binaryParse)
    // .map(Integer.bitCount)
    .map(x => x > linesCount)
    
  val gammaVal = binaryParse(gamma.map(x => if (x) "1" else "0")
    .mkString)
  val epsilon = binaryParse(gamma.map(x => if (x) "0" else "1")
    .mkString)
  println(gamma)

  (gammaVal, epsilon)

def partOne: Unit = 
  def text = Source.fromFile(filename).getLines.toList
  val (gamma, epsilon) = decode(text)
  println(gamma * epsilon)


@main def hello: Unit = 
  partOne

