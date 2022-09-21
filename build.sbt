version := "1.0"

scalaVersion := "2.13.6"

name :="Algosiki"

libraryDependencies += "io.monix" %% "monix" % "3.4.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"

enablePlugins(JmhPlugin)