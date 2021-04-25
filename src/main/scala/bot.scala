import zio.*, nio.*, core.*, clock.*, stream.*, console.*, system.*, IO.succeed
import db.*
import ftier.*, ws.*, udp.*, http.*
import proto.*

val app =
  (for {
    envport <- env("botport")
    port = envport.flatMap(_.toIntOption).getOrElse(8002)
    _ <- putStrLn(s"http://localhost:$port")
    addr <- SocketAddress.inetSocketAddress(port)
    store <- ZIO.environment[Store & ZEnv]
    _  <- http.server.bind[Store & ZEnv](
            addr,
            httpHandler,
            _ => IO.unit,
          )
  } yield ()).provideCustomLayer(Store.live("data"))

@main def run(): Unit = Runtime.default.unsafeRun(app)
