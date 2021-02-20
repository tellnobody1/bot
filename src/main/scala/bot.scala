import zero.ext._, option._
import zio._, nio._, core._, clock._, stream._, console._
import db._
import ftier._, ws._, udp._, http._
import zd.proto.api._

val app =
  (for {
    port <- IO.succeed(8002)
    _ <- putStrLn(s"http://localhost:$port")
    addr <- SocketAddress.inetSocketAddress(port)
    store <- ZIO.environment[Store with ZEnv]
    _  <- httpServer.bind(
            addr,
            IO.succeed(httpHandler.andThen(_.provide(store))),
            IO.succeed(PartialFunction.empty),
          )
  } yield ()).provideCustomLayer(Store.live("data"))

@main def run(): Unit = Runtime.default.unsafeRun(app)