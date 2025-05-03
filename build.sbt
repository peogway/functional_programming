import Dependencies._

ThisBuild / scalaVersion     := "2.13.12"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "REPS",
    libraryDependencies ++= Seq(
      munit % Test,
      "io.circe" %% "circe-core" % "0.14.1",    // Core library
      "io.circe" %% "circe-parser" % "0.14.1",   // JSON parser
      "io.circe" %% "circe-literal" % "0.14.1" , // Literal support
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
