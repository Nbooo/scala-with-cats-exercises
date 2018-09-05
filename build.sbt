name := "scala-with-cats"

version := "0.1"

scalaVersion := "2.12.6"
scalacOptions += "-Ypartial-unification"
scalacOptions += "-language:higherKinds"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.2.0"