name := "TP Scala - teden 9."

version := "1.0"

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "2.4" % "test",
    "org.twitter4j" % "twitter4j-stream" % "4.0.2",
    "org.twitter4j" % "twitter4j-async" % "4.0.2",
    "com.kennycason" % "kumo" % "1.1",
    "com.github.wookietreiber" %% "scala-chart" % "latest.integration"
  )

org.scalastyle.sbt.ScalastylePlugin.Settings
