resolvers += Resolver.jcenterRepo
resolvers += Resolver.githubPackages("zero-deps")
addSbtPlugin("com.codecommit" % "sbt-github-packages" % "latest.integration")
addSbtPlugin("io.github.zero-deps" % "sbt-git" % "latest.integration")
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "latest.integration")
addSbtPlugin("com.github.shmishleniy" % "sbt-deploy-ssh" % "0.1.4")

/* publishing */
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "latest.integration")
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "latest.integration")
/* publishing */
