import zero.ext._, option._
import zio._, nio._, core._, clock._, stream._, console._, system._
import db._
import ftier._, ws._, udp._, http._
import zd.proto.api._

val httpHandler: PartialFunction[Request, ZIO[Store with ZEnv, Nothing, Response]] = {
  case Request("GET", "/acpo", _, _) => IO.succeed(response(acpoHtml, "text/html"))
  case Request("POST", url, _, _) if url startsWith "/acpo_link" => ???
  case Request("POST", url, _, body) if url startsWith "/bot" =>
    (for {
      secret <- ZIO.require(NoSecret)(env("botsecret"))
    } yield response(???, "application/json")).catchAll(e => for {
      _ <- putStrLn(e.toString)
    } yield notfound)
}

val notfound =
  val content = Chunk.fromArray("Not Found".getBytes("utf8").nn)
  Response(404, headers(content.length, "text/plain"), content)

def response(content: Chunk[Byte], ct: String): Response =
  Response(200, headers(content.length, ct), content)

def headers(len: Long, ct: String): Map[String, String] =
  Map(
    "Content-Length" -> len.toString
  , "Content-Type" -> ct
  )

object NoSecret
type NoSecret = NoSecret.type