name := "Akka Boids"

version := "1.0"

scalaVersion := "2.11.2"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.6"

libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.3.6"

libraryDependencies += "org.scala-lang.modules" % "scala-swing_2.11" % "1.0.1"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"