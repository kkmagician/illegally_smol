name := "illegallySmol"

version := "0.2"

scalaVersion := "2.13.2"

val circeVersion = "0.13.0"
val http4sVersion = "0.21.3"

libraryDependencies ++= Seq(
  "net.debasishg" %% "redisclient" % "3.20",
  "org.slf4j" % "slf4j-api" % "1.7.30",
  "org.slf4j" % "slf4j-simple" % "1.7.30"
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

packageName in Docker := "illegallysmolcats"
version in Docker := version.value
dockerBaseImage := "openjdk:15-oracle"
dockerRepository := Some("registry.gitlab.com")
dockerUsername := Some("awfrke")
dockerUpdateLatest := true
