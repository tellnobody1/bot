scalaVersion := "3.0.0-RC1"
version := zero.git.version()

libraryDependencies ++= Seq(
  "dev.zio" %% "zio-streams"  % "1.0.5"
, "dev.zio" %% "zio-test-sbt" % "1.0.5" % Test
)
testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

lazy val db = project.in(file("deps/db"))
lazy val frontier = project.in(file("deps/frontier"))
dependsOn(db, frontier)

fork := true

scalacOptions ++= Seq(
  "-language:postfixOps"
, "-language:strictEquality"
, "-Yexplicit-nulls"
, "-source", "future-migration"
, "-deprecation"
, "-rewrite"
, "release", "15"
)

enablePlugins(JavaAppPackaging, DeploySSH)
mappings in (Compile, packageDoc) := Seq()

turbo := true
useCoursier := true
Global / onChangedBuildSource := ReloadOnSourceChanges

resolvers += Resolver.JCenterRepository

import deployssh.DeploySSH.{ServerConfig, ArtifactSSH}
import fr.janalyse.ssh.SSH
deployConfigs += ServerConfig(name="server", host="server", user=Some("ubuntu"))
deployArtifacts += ArtifactSSH((Universal / packageBin).value, "prj/bot")
deploySshExecBefore ++=
  Seq(
    (ssh: SSH) => ssh.shell{ shell =>
      shell.execute("cd prj/bot")
      shell.execute("touch pid")
      val pid = shell.execute("cat pid")
      if (pid.nonEmpty) {
        shell.execute(s"kill ${pid}; sleep 1000; kill -9 ${pid}")
        shell.execute("rm pid")
      }
    }
  )
deploySshExecAfter ++=
  Seq(
    (ssh: SSH) => ssh.shell{ shell =>
      val name = (packageName in Universal).value
      shell.execute("cd prj/bot")
      shell.execute(s"rm $name")
      shell.execute(s"unzip -q -o ${name}.zip")
      shell.execute(s"rm *.zip")
      shell.execute(s"nohup ./${name}/bin/bot &")
      shell.execute("echo $! > pid")
    }
  )
