scalaVersion := "2.11.6"

// https://github.com/tkawachi/sbt-doctest
doctestSettings
doctestWithDependencies := false
doctestTestFramework := DoctestTestFramework.ScalaTest

//scalacOptions += "-Xfatal-warnings -feature"
scalacOptions += "-feature"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "com.typesafe.akka" %% "akka-actor" % "2.3.11",
  "com.typesafe.akka" %% "akka-stream-experimental" % "1.0-RC2",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
)
