name := "ambient-i"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies += "com.illposed.osc" % "javaosc-core" % "0.2"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.0.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2"

libraryDependencies += "net.soundmining" %% "soundmining-tools" % "1.0-SNAPSHOT"

org.scalastyle.sbt.ScalastylePlugin.Settings
