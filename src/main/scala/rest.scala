import zero.ext._, option._
import zio._, nio._, core._, clock._, stream._, console._
import db._
import ftier._, ws._, udp._, http._
import zd.proto.api._

val httpHandler: PartialFunction[Request, ZIO[Store, Err, Response]] = {
  case Request("GET", "/acpo", _, _) => IO.succeed(response(acpoHtml, "text/html"))
}

def response(content: Chunk[Byte], ct: String): Response =
  Response(200, headers(content.length, ct), content)

def headers(len: Long, ct: String): Map[String, String] =
  Map(
    "Content-Length" -> len.toString
  , "Content-Type" -> ct
  )
