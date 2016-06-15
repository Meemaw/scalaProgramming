name := "TP Scala - teden 7. - twittertime"

version := "1.0"

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "2.4" % "test",
   "org.twitter4j" % "twitter4j" % "4.0.2"
  )


org.scalastyle.sbt.ScalastylePlugin.Settings
