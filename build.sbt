lazy val bot = project.in(file("."))
  .settings(
    scalaVersion := "3.0.0-RC2"
  , libraryDependencies ++= Seq(
      "dev.zio" %% "zio-streams"  % "1.0.6"
    , "dev.zio" %% "zio-test-sbt" % "1.0.6" % Test
    , "org.apache.poi" % "poi-ooxml" % "5.0.0"
    )
  , testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  , fork := true
  , scalacOptions ++= Seq(
      "-language:postfixOps"
    , "-language:strictEquality"
    , "-Yexplicit-nulls"
    , "-source", "future-migration" , "-deprecation"
    , "release", "11"
    )
  ).dependsOn(db, ftier).enablePlugins(PackPlugin, DeploySSH)

lazy val db = project.in(file("deps/db"))

lazy val ftier = project.in(file("deps/frontier"))

import deployssh.DeploySSH.{ServerConfig, ArtifactSSH}
import fr.janalyse.ssh.SSH
deployConfigs += ServerConfig(name="server", host="server", user=Some("ubuntu"))
deployArtifacts += ArtifactSSH(packArchiveTgz.value, "prj/bot")
deploySshExecBefore ++=
  Seq(
    (ssh: SSH) => ssh.shell{ shell =>
      shell.execute("cd prj/bot")
      shell.execute("touch pid")
      val pid = shell.execute("cat pid")
      if (pid.nonEmpty) {
        shell.execute(s"kill ${pid}")
        shell.execute("rm pid")
      }
    }
  )
deploySshExecAfter ++=
  Seq(
    (ssh: SSH) => ssh.shell{ shell =>
      val name = packArchiveName.value
      shell.execute("cd prj/bot")
      shell.execute(s"rm $name")
      shell.execute(s"rm nohup.out")
      shell.execute(s"tar -xzf ${name}.tar.gz")
      shell.execute(s"rm *.tar.gz")
      shell.execute(s"nohup ./${name}/bin/run &")
      shell.execute("echo $! > pid")
    }
  )
packGenerateWindowsBatFile := false
packGenerateMakefile := false

turbo := true
useCoursier := true
Global / onChangedBuildSource := ReloadOnSourceChanges
