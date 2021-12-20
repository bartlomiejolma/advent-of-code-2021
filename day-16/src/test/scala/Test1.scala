import org.junit.Test
import org.junit.Assert.*

class Test1:
  @Test def tParseHex(): Unit = 
    assertEquals("110100101111111000101000", parseHex("D2FE28"))
    assertEquals("00111000000000000110111101000101001010010001001000000000", parseHex("38006F45291200"))
    assertEquals("11101110000000001101010000001100100000100011000001100000", parseHex("EE00D40C823060"))


  @Test def tParsePacket(): Unit =
      assertEquals((LiteralValue(BigInt(6), BigInt(2021)), "000"), parsePacket("110100101111111000101000"))

  @Test def tParsePacketOperator(): Unit =

      val (parsed, remaining) = parsePacket("00111000000000000110111101000101001010010001001000000000")
      assertEquals(BigInt(1), parsed.version)
      parsed match {
        case Operator(_, subPackets, _) =>
          assertEquals(2, subPackets.length)
          assertEquals(LiteralValue(BigInt(6), BigInt(10)), subPackets(0))
          assertEquals(LiteralValue(BigInt(2), BigInt(20)), subPackets(1))
        case _ => fail("Expected Operator")
      }
      assertEquals("0000000", remaining)

  @Test def tParsePacketOperatorMultiple(): Unit =

      val (parsed, remaining) = parsePacket("11101110000000001101010000001100100000100011000001100000")
      assertEquals(BigInt(7), parsed.version)
      parsed match {
        case Operator(_, subPackets, _) =>
          assertEquals(3, subPackets.length)
          assertEquals(LiteralValue(BigInt(2), BigInt(1)), subPackets(0))
          assertEquals(LiteralValue(BigInt(4), BigInt(2)), subPackets(1))
          assertEquals(LiteralValue(BigInt(1), BigInt(3)), subPackets(2))
        case _ => fail("Expected Operator")
      }
      assertEquals("00000", remaining)

  @Test def tGetVersion(): Unit =
    val binary = parseHex("8A004A801A8002F478")
    val (packet, _) = parsePacket(binary)
    println(packet)
    assertEquals(BigInt(16), getVersionSum(packet))

  @Test def tGetVersion2(): Unit =
    val binary = parseHex("620080001611562C8802118E34")
    val (packet, _) = parsePacket(binary)
    println(packet)
    assertEquals(BigInt(12), getVersionSum(packet))

  @Test def tPartOne(): Unit =
    val input = """8A004A801A8002F478
620080001611562C8802118E34
C0015000016115A2E0802F182340
A0016C880162017C3686B18A3D4780
"""
    assertEquals(BigInt(82), partOne(input))
  

  @Test def tApplyOperator(): Unit =
    val binary = parseHex("C200B40A82")
    val (packet, _) = parsePacket(binary)
    println(packet)
    assertEquals(BigInt(3), applyOperator(packet))

  @Test def tApplyOperator2(): Unit =
    val binary = parseHex("04005AC33890")
    val (packet, _) = parsePacket(binary)
    println(packet)
    assertEquals(BigInt(54), applyOperator(packet))

  @Test def tApplyOperator3(): Unit =
    val binary = parseHex("880086C3E88112")
    val (packet, _) = parsePacket(binary)
    println(packet)
    assertEquals(BigInt(7), applyOperator(packet))

  @Test def tApplyOperator4(): Unit =
    val binary = parseHex("D8005AC2A8F0")
    val (packet, _) = parsePacket(binary)
    println(packet)
    assertEquals(BigInt(1), applyOperator(packet))

  @Test def tApplyOperator5(): Unit =
    val binary = parseHex("F600BC2D8F")
    val (packet, _) = parsePacket(binary)
    println(packet)
    assertEquals(BigInt(0), applyOperator(packet))

  @Test def tApplyOperator6(): Unit =
    val binary = parseHex("9C0141080250320F1802104A08")
    val (packet, _) = parsePacket(binary)
    println(packet)
    assertEquals(BigInt(1), applyOperator(packet))