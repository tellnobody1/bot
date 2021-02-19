import zio._, console._

val app =
  for {
    _ <- putStrLn("hi")
  } yield ()

@main def run(): Unit = Runtime.default.unsafeRunAsync(app)(_ => ())