scalaVersion := "2.11.6"

// https://github.com/tkawachi/sbt-doctest
doctestSettings
doctestWithDependencies := false
doctestTestFramework := DoctestTestFramework.ScalaTest

//scalacOptions += "-Xfatal-warnings -feature"
scalacOptions += "-feature"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
)
