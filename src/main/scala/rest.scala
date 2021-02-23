import java.security.SecureRandom
import zero.ext._, option._
import zio._, nio._, core._, clock._, stream._, console._, system._
import db._
import ftier.{telegram=>tg,_}, ws._, udp._, http._
import zd.proto._, api.MessageCodec, macrosapi._

val httpHandler: PartialFunction[Request, ZIO[Store with ZEnv, Err, Response]] = {
  case Request("GET", Root / "acpo", _, _) => IO.succeed(response(acpoHtml, "text/html"))

  case Request("POST", (Root / "acpo" / "link") ? ("id"*id & "fiz_id"*fiz_id & "token"*token), _, _) =>
    for {
      chatid <- ZIO.require(Attack)(get(Key(id.utf8)))
      dat <- ((fiz_id, token)).encode.map(Dat.apply)
      _ <- put(chatid.toKey, dat)
    } yield response("Готово.".toChunk, "text/plain")

  case Request("POST", Root / "bot" / x, _, body) =>
    for {
      secret <- ZIO.require[ZEnv, Err, String](NoSecret)(env("botsecret"))
      _ <- IO.when(x != secret)(IO.fail(Attack))
      answer <-
        tg.reader.find[Store with ZEnv, Err](body) {
          case tg.Update.PrivateQuery(chatid, "/start") =>
            for {
              r <- IO.effectTotal(SecureRandom())
              xs <- IO.succeed(new Array[Byte](32))
              _ <- IO.effectTotal(r.nextBytes(xs))
              id <- IO.effectTotal(xs.hex)
              _ <- put(Key(id), Dat(chatid.toBytes))
              url = s"https://bot2.nobodytells.me/acpo?id=${id.utf8}"
              a  <- tg.writer.answerPrivateQuery(chatid, tg.QueryRes(url))
            } yield a
          case tg.Update.PrivateQuery(chatid, "acpo") =>
            for {
              dat <- ZIO.require(Attack)(get(Key(chatid.toBytes)))
              ft <- dat.bytes.decode[(String,String)]
              (fiz_id, token) = ft
              a  <- tg.writer.answerPrivateQuery(chatid, tg.QueryRes("спробуйте пізніше"))
            } yield a
          case x => IO.fail(NotImplemented(x))
        }
    } yield response(answer, "application/json")
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
case class NotImplemented(x: tg.Update)

type Err = NoSecret.type | Attack.type | NotImplemented | SecurityException

given MessageCodec[Tuple2[String,String]] = caseCodecIdx

given CanEqual[String, tg.Query] = CanEqual.derived
