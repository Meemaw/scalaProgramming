name := "TP Scala - teden 11."

version := "1.0"

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "2.4" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.2"% "test"
  )

org.scalastyle.sbt.ScalastylePlugin.Settings
