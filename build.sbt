organization := "scala"

name := "Galileo"

version := "1.0"

scalaVersion := "2.11.1"

scalacOptions ++= Seq( "-deprecation", "-feature" )

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "org.scala-lang" % "jline" % "2.9.0-1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"


