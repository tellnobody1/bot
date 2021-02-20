import java.security.SecureRandom
import zero.ext._, option._
import zio._, nio._, core._, clock._, stream._, console._, system._
import db._
import ftier.{telegram=>tg,_}, ws._, udp._, http._
import zd.proto.api._

val httpHandler: PartialFunction[Request, ZIO[Store with ZEnv, Nothing, Response]] = {
  case Request("GET", url, _, _) if url startsWith "/acpo?" => IO.succeed(response(acpoHtml, "text/html"))

  case Request("POST", url, _, _) if url startsWith "/acpo_link?" => IO.succeed(notfound) //todo

  case Request("POST", url, _, body) if url startsWith "/bot" =>
    (for {
      secret <- ZIO.require(NoSecret)(env("botsecret"))
      _ <- IO.when(url != s"/bot$secret")(IO.fail(Attack))
      answer <- tg.reader.find(body) {
                  case tg.Update.PrivateQuery(chatId, q) if q.isStart =>
                    for {
                      r <- IO.effectTotal(new SecureRandom)
                      xs <- IO.succeed(new Array[Byte](32))
                      _ <- IO.effectTotal(r.nextBytes(xs))
                      hex <- IO.effectTotal(xs.hex)
                      a <- tg.writer.answerPrivateQuery(chatId, tg.QueryRes(s"http://localhost:8002/acpo_link?id=$hex"))
                    } yield a
                  case x => IO.fail(NotImplemented(x))
                }
    } yield response(answer, "application/json")).catchAll(e => for {
      _ <- putStrLn(e.toString)
    } yield notfound)
}

val notfound =
  val content = "Not Found".toChunk
  Response(404, headers(content.length, "text/plain"), content)

def response(content: Chunk[Byte], ct: String): Response =
  Response(200, headers(content.length, ct), content)

def headers(len: Long, ct: String): Map[String, String] =
  Map(
    "Content-Length" -> len.toString
  , "Content-Type" -> ct
  )

object NoSecret
object Attack
class NotImplemented(x: tg.Update)