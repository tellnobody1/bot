import java.security.SecureRandom
import zero.ext._, option._
import zio._, nio._, core._, clock._, stream._, console._, system._
import db._
import ftier.{telegram=>tg,_}, ws._, udp._, http._
import zd.proto.api._

val httpHandler: PartialFunction[Request, ZIO[Store with ZEnv, Nothing, Response]] = {
  case Request("GET", Root / "acpo", _, _) => IO.succeed(response(acpoHtml, "text/html"))

  case Request("POST", (Root / "acpo" / "link") ? ("id"*id & "fiz_id"*fiz_id & "token"*token), _, _) =>
    IO.succeed(notfound) //todo

  case Request("POST", Root / "bot" / x, _, body) => //todo: change webhook
    (for {
      secret <- ZIO.require(NoSecret)(env("botsecret"))
      _ <- IO.when(x != secret)(IO.fail(Attack))
      answer <- tg.reader.find(body) {
                  case tg.Update.PrivateQuery(chatId, q) if q.isStart =>
                    for {
                      r <- IO.effectTotal(new SecureRandom)
                      xs <- IO.succeed(new Array[Byte](32))
                      _ <- IO.effectTotal(r.nextBytes(xs))
                      hex <- IO.effectTotal(xs.hex)
                      //todo store.put(hex, chatId)
                      url = s"https://bot.nobodytells.me/acpo?id=$hex"
                      a  <- tg.writer.answerPrivateQuery(chatId, tg.QueryRes(url))
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
