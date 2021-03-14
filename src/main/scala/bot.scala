import zero.ext._, option._
import zio._, nio._, core._, clock._, stream._, console._, system._, IO.succeed
import db._
import ftier.{Err=>_,_}, ws._, udp._, http._
import zd.proto.api._

val app =
  (for {
    envport <- env("botport")
    port = envport.flatMap(_.toIntOption).getOrElse(8002)
    _ <- putStrLn(s"http://localhost:$port")
    addr <- SocketAddress.inetSocketAddress(port)
    store <- ZIO.environment[Store with ZEnv]
    _  <- httpServer.bind(
            addr,
            succeed(httpHandler
              .andThen(
                _.tapError{
                  case Attack => IO.unit //putStrLn("attack is detected")
                  case NoSecret => putStrLn("bot secret is missing")
                  case x: ftier.Err => putStrLn(x.toString) //todo: http client err
                }
              ).andThen(
                _.catchAll(_ => succeed(notfound))
              ).andThen(
                _.provide(store)
              )
            ),
            succeed(PartialFunction.empty),
          )
  } yield ()).provideCustomLayer(Store.live("data"))

@main def run(): Unit = Runtime.default.unsafeRun(app)

given CanEqual[Err, Err] = CanEqual.derived