import java.security.SecureRandom
import zio.*, nio.*, core.*, clock.*, stream.*, console.*, system.*
import zio.IO.{succeed, effectTotal, when, fail, effect}
import db.*
import ftier.*, tg.*, ws.*, udp.*, http.*, server.*, client.*, json.*
import proto.*

val httpHandler: HttpHandler[Store & ZEnv] = {
  case Request("GET", Root / "acpo", _, _) => IO.succeed(response(acpoHtml, "text/html"))

  case Request("POST", (Root / "acpo" / "link") ? ("id"*id & "fiz_id"*fiz_id & "token"*token), _, _) =>
    for {
      chatid <- ZIO.require(Attack)(get(Key(id.utf8)))
      dat <- (fiz_id, token).bencode.map(Dat.apply)
      _ <- put(chatid.toKey, dat)
    } yield response("Готово.".toChunk, "text/plain")

  case Request("POST", Root / "bot" / x, _, body) =>
    for {
      secret <- ZIO.require(NoSecret)(env("botsecret").catchAll(_ => IO.none)) //todo: pass as param
      _ <- when(x != secret)(fail(Attack))
      answer <-
        reader.find(body) { //todo: remove this callback?
          case Update.PrivateQuery(chatid, "/start") =>
            writer.answerPrivateQuery(chatid, QueryRes("вітаю"),
              Some(ReplyKeyboardMarkup(
                ("acpo/login" :: "acpo/get" :: Nil) ::
                Nil
              ))
            )

          case Update.PrivateQuery(chatid, "/help") =>
            val url = "https://github.com/tellnobody1/bot/issues"
            writer.answerPrivateQuery(chatid, QueryRes(s"""<a href="$url">підтримок</a>"""))

          case Update.PrivateQuery(chatid, "acpo/login") =>
            for {
              r <- effectTotal(SecureRandom())
              xs <- succeed(new Array[Byte](32))
              _ <- effectTotal(r.nextBytes(xs))
              id <- effectTotal(xs.hex)
              _ <- put(Key(id), Dat(chatid.toBytes))
              url = s"https://bot.tellnobody.space/acpo?id=${id.utf8}"
              a  <- writer.answerPrivateQuery(chatid, QueryRes(url))
            } yield a

          case Update.PrivateQuery(chatid, "acpo/get") =>
            for {
              dat <- get(Key(chatid.toBytes))
              res <-
                dat match
                  case Some(dat) =>
                    for {
                      ft <- dat.bytes.bdecode[(String,String)]
                      (fiz_id, token) = ft
                      cp <- connectionPool
                      content = s"""{"fiz_id":$fiz_id}""".toChunk
                      hs = Map(
                        "Content-Type" -> "application/json"
                      , "Authorization" -> s"Bearer $token"
                      )
                      r <- http.client.sendAsync(cp, Request("POST", "https://portal.acpo.com.ua/fiz/alldata", hs, content))
                      tree <- jtree(r.body)
                      in_sum <- effect(tree.findPath("in_sum").nn.asDouble).orDie
                      bal_sum_usd <- effect(tree.findPath("bal_sum_usd").nn.asDouble).orDie
                      inv_percent_usd <- effect(tree.findPath("inv_percent_usd").nn.asDouble).orDie
                      inv_sum_usd <- effect(tree.findPath("inv_sum_usd").nn.asDouble).orDie
                    } yield s"""
                        |інвестовано: ${f"$in_sum%.2f₴"}
                        |баланс: ${f"$$$bal_sum_usd%.2f"}
                        |дохідність: ${f"$inv_percent_usd%%"}
                        |дохід: ${f"$$$inv_sum_usd%.2f"}
                        """.stripMargin.stripPrefix("\n").stripSuffix("\n")
                  case None =>
                    succeed("виконайте acpo/login")
              a <- writer.answerPrivateQuery(chatid, QueryRes(res))
            } yield a

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

object NoSecret extends Throwable
object Attack extends Throwable

given MessageCodec[Tuple2[String,String]] = caseCodecIdx

given CanEqual[String, tg.Query] = CanEqual.derived
given CanEqual[None.type, Option[Dat]] = CanEqual.derived
