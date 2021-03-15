import zio.*

extension (x: String)
  inline def toChunk: Chunk[Byte] = Chunk.fromArray(x.getBytes("utf8").nn)
