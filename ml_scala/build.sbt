name := "ml_scala"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

scalacOptions += "-deprecation"
scalacOptions += "-feature"
scalacOptions += "-language:implicitConversions"
