import com.typesafe.sbt.SbtGit._

name := "gwen"

description := "A Given-When-Then interpreter and automation platform"

organization := "org.gweninterpreter"

organizationHomepage := Some(url("http://gweninterpreter.org"))

startYear := Some(2014)

scalaVersion := "2.12.1"

crossPaths := false

scalacOptions += "-feature"

scalacOptions += "-language:postfixOps"

scalacOptions += "-deprecation"

scalacOptions += "-target:jvm-1.8"

licenses += "Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html")

homepage := Some(url("https://github.com/gwen-interpreter/gwen"))

javaSource in Compile := baseDirectory.value / "src/main/scala"

javaSource in Test := baseDirectory.value / "src/test/scala"

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "io.cucumber" % "gherkin" % "4.0.0"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.0-M1"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"

libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.7.22"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"

libraryDependencies += "jline" % "jline" % "2.14.2"

libraryDependencies += "commons-codec" % "commons-codec" % "1.10"

libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.4"

libraryDependencies += "com.jayway.jsonpath" % "json-path" % "2.2.0"

libraryDependencies += "joda-time" % "joda-time" % "2.9.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "org.mockito" % "mockito-all" % "1.10.19" % "test"

mappings in (Compile, packageBin) ++= Seq(
  file("LICENSE") -> "LICENSE",
  file("NOTICE") -> "NOTICE",
  file("LICENSE-THIRDPARTY") -> "LICENSE-THIRDPARTY",
  file("CHANGELOG") -> "CHANGELOG"
)
