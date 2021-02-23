import java.security.SecureRandom
import zero.ext._, option._
import zio._, nio._, core._, clock._, stream._, console._, system._
import zio.IO.{succeed, effectTotal, when, fail}
import db._
import ftier._, tg._, ws._, udp._, http._
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
      secret <- ZIO.require[ZEnv, Err, String](NoSecret)(env("botsecret").catchAll(_ => IO.none)) //todo: pass as param
      _ <- when(x != secret)(fail(Attack))
      answer <-
        reader.find[Store with ZEnv, Err](body) {
          case Update.PrivateQuery(chatid, "/start") =>
            writer.answerPrivateQuery(chatid, QueryRes("Вітаю"),
              ReplyKeyboardMarkup(
                ("acpo/login" :: "acpo/get" :: Nil) ::
                Nil
              ).some
            )

          // case Update.PrivateQuery(chatid, "acpo/login") =>
          //   for {
          //     r <- effectTotal(SecureRandom())
          //     xs <- succeed(new Array[Byte](32))
          //     _ <- effectTotal(r.nextBytes(xs))
          //     id <- effectTotal(xs.hex)
          //     _ <- put(Key(id), Dat(chatid.toBytes))
          //     url = s"https://bot2.nobodytells.me/acpo?id=${id.utf8}"
          //     a  <- writer.answerPrivateQuery(chatid, QueryRes(url))
          //   } yield a

          // case Update.PrivateQuery(chatid, "acpo/get") =>
          //   for {
          //     dat <- get(Key(chatid.toBytes))
          //     res <-
          //       dat match
          //         case Some(dat) =>
          //           for {
          //             ft <- dat.bytes.decode[(String,String)]
          //             (fiz_id, token) = ft
          //           } yield "наразі функціонал не працює"
          //         case None =>
          //           succeed("виконайте acpo/login знову")
          //     a <- writer.answerPrivateQuery(chatid, QueryRes(res))
          //   } yield a

          case x =>
            for {
              _ <- putStrLn(x.toString)
            } yield "{}".toChunk
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

type Err = NoSecret.type | Attack.type

given MessageCodec[Tuple2[String,String]] = caseCodecIdx

given CanEqual[String, tg.Query] = CanEqual.derived
given CanEqual[None.type, Option[Dat]] = CanEqual.derived