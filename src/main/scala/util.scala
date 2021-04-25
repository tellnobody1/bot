import zio.*

extension (x: String)
  inline def toChunk: Chunk[Byte] = Chunk.fromArray(x.getBytes("utf8").nn)
  inline def utf8: IArray[Byte] = IArray.unsafeFromArray(x.getBytes("utf8").nn)

extension (x: Array[Byte])
  @annotation.targetName("hex2") inline def hex: IArray[Byte] = IArray.unsafeFromArray(x).hex

extension (x: IArray[Byte])
  def hex: IArray[Byte] =
    val acc = new Array[Byte](x.length * 2)
    var i = 0
    while (i < x.length) {
      val v = x(i) & 0xff
      acc(i * 2) = hexs(v >>> 4)
      acc(i * 2 + 1) = hexs(v & 0x0f)
      i += 1
    }
    IArray.unsafeFromArray(acc)

private val hexs = "0123456789abcdef".getBytes("ascii").nn

extension [A](x: A)
  inline def bencode(using proto.MessageCodec[A]): UIO[IArray[Byte]] = IO.effectTotal(proto.encodeI(x))

extension (x: IArray[Byte])
  inline def utf8: String = String(x.toArray, "utf8")
  inline def bdecode[A](using proto.MessageCodec[A]): UIO[A] = IO.effect(proto.decodeI(x)).orDie
