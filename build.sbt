name := "illegallySmol"

version := "0.1"

scalaVersion := "2.13.2"

val circeVersion = "0.13.0"
val http4sVersion = "0.21.3"

libraryDependencies ++= Seq(
  "net.debasishg" %% "redisclient" % "3.20"
) ++ Seq(
  "org.http4s" %% "http4s-dsl",
  "org.http4s" %% "http4s-blaze-client",
  "org.http4s" %% "http4s-circe"
).map(_ % http4sVersion) ++ Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-generic-extras",
  "io.circe" %% "circe-optics"
).map(_ % circeVersion)

scalacOptions ++= Seq("-Ymacro-annotations")

enablePlugins(DockerPlugin)
enablePlugins(JavaAppPackaging)

packageName in Docker := "registry.gitlab.com/awfrke/illegallysmolcats"
dockerBaseImage := "openjdk:15-oracle"
