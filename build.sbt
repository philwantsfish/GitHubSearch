name := "GitHubSearch"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq("org.scalatest" % "scalatest_2.10" % "2.0" % "test",
  "org.jsoup" % "jsoup" % "1.8.3",
  "io.spray" %%  "spray-json" % "1.3.2",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
  "ch.qos.logback" %  "logback-classic" % "1.1.7"
)