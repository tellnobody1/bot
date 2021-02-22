scalaVersion := "3.0.0-M3"
version := zero.git.version()

libraryDependencies ++= Seq(
  "dev.zio" %% "zio-streams"  % "1.0.4-2"
, "dev.zio" %% "zio-test-sbt" % "1.0.4-2" % Test
)
testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

lazy val db = project.in(file("deps/db"))
lazy val frontier = project.in(file("deps/frontier"))
dependsOn(db, frontier)

fork := true

scalacOptions ++= Seq(
  "-language:postfixOps"
, "-Yexplicit-nulls"
, "-language:strictEquality"
)

enablePlugins(JavaAppPackaging)
mappings in (Compile, packageDoc) := Seq()

turbo := true
useCoursier := true
Global / onChangedBuildSource := ReloadOnSourceChanges

resolvers += Resolver.JCenterRepository
