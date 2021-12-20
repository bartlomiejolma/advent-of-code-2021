import scala.io.Source

val filename = "input.txt"

def parseHex(hex: String) : String = 
  hex.grouped(1).map( char => 
    String.format("%4s", BigInt(char, 16).toString(2)).replace(' ', '0')).mkString

trait Packet {
  def version: BigInt
}

case class LiteralValue (version: BigInt, value: BigInt) extends Packet

case class Operator (version: BigInt, subPackets: List[Packet], operationId: BigInt) extends Packet 

 
def parseLiteralValue(binaryString: String, version: BigInt) : (LiteralValue, String) = 
  val groups = binaryString.grouped(5).toList

  def predicate = (x: String) => x(0) == '1'
  val valueGroups = groups.takeWhile(predicate).toList
  val nonValueGroups = groups.dropWhile(predicate).toList

  val groupsWithValue = valueGroups ::: List[String](nonValueGroups.head)

  val value = BigInt(groupsWithValue.map(_.substring(1, 5)).mkString, 2)
  return (LiteralValue(version, value), nonValueGroups.tail.mkString)

def parseSubPacketsFromBits(binaryString: String): (List[Packet], String) =
  val length = 15
  val bitsLength = BigInt(binaryString.substring(0, length), 2)
  val subPacketsBinaryString = binaryString.substring(length, length + bitsLength.toInt)

  var stringsToParse = subPacketsBinaryString
  var subPackets = List[Packet]()
  while (stringsToParse.length > 0) {
    val (packet, remaining) = parsePacket(stringsToParse)
    stringsToParse = remaining
    subPackets = packet :: subPackets
  }
  (subPackets.reverse, binaryString.substring(length + bitsLength.toInt))

def parseSubPacketsFromNumber(binaryString: String): (List[Packet], String) =
  val length = 11
  val number = BigInt(binaryString.substring(0, length), 2)
  var remaining = binaryString.substring(length)

  val subPackets = (1 to number.toInt).map(_ => 
    val (packet, remainingPart) = parsePacket(remaining)  
    remaining = remainingPart
    packet
  )
  (subPackets.toList, remaining)

def parseOperator(binaryString: String, version: BigInt, operationId: BigInt) : (Operator, String) = {
  val lengthTypeId = binaryString.substring(0, 1)
  val (subPackets, rest) =  lengthTypeId match {
    case "0" => parseSubPacketsFromBits(binaryString.substring(1))
    case _ => parseSubPacketsFromNumber(binaryString.substring(1))
  }
  return (Operator(version, subPackets, operationId), rest)
}

def parsePacket(binaryString: String) : (Packet, String) = {
  val version = BigInt(binaryString.substring(0, 3), 2)
  val packetType = BigInt(binaryString.substring(3, 6), 2)
  println(s"Parsing packet with version $version and type $packetType")
  println(s"Binary string: $binaryString")
  val rest = binaryString.substring(6, binaryString.length)
  packetType match {
    case 4 => parseLiteralValue(rest, version)
    case operationId => parseOperator(rest, version, operationId)
  }
}

def getVersion(packet: Packet) : List[BigInt] = packet match {
  case LiteralValue(version, _) => List[BigInt](version)
  case Operator(version, subPackets, _) => subPackets.flatMap(getVersion) ::: List[BigInt](version)
}

def getVersionSum(packet: Packet) : BigInt = getVersion(packet).sum

// def addition =
val operations = Map[BigInt, (BigInt, BigInt) => BigInt](
  BigInt(0) -> ((prev: BigInt, next:BigInt) => prev + next),
  BigInt(1) -> ((prev: BigInt, next: BigInt) => prev * next),
  BigInt(2) -> ((prev: BigInt, next: BigInt) => List[BigInt](prev, next).min),
  BigInt(3) -> ((prev: BigInt, next: BigInt) => List[BigInt](prev, next).max),
  BigInt(5) -> ((prev: BigInt, next: BigInt) => (if (prev > next) BigInt(1)  else BigInt(0))),
  BigInt(6) -> ((prev: BigInt, next: BigInt) => (if (prev < next) BigInt(1)  else BigInt(0))),
  BigInt(7) -> ((prev: BigInt, next: BigInt) => (if (prev == next) BigInt(1)  else BigInt(0))),

)

def applyOperator(packet: Packet): BigInt = packet match {
  case Operator(_, subPackets, operationId) => 
    val subPacketsValues = subPackets.map(applyOperator)
    val operation = operations(operationId)
    subPacketsValues.reduceLeft(operation)
  case LiteralValue(_, value) => value
}

def partOne(text: String): BigInt = 
  text.split("\n").map(parseHex).map(parsePacket).map(_._1).map(getVersionSum).sum
  
def partTwo(text: String): BigInt = 
  text.split("\n").map(parseHex).map(parsePacket).map(_._1).map(applyOperator).sum
  

@main def hello: Unit = 
  def text = Source.fromFile(filename).mkString
  println(partOne(text))
  println(partTwo(text))
