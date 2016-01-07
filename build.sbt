import com.typesafe.sbt.SbtGit._

name := "gwen"

description := "Executable Given-When-Then specifications"

organization := "org.gweninterpreter"

organizationHomepage := Some(url("http://gweninterpreter.org"))

startYear := Some(2014)

scalaVersion := "2.11.7"

crossScalaVersions := Seq("2.11.7", "2.10.6")

scalacOptions += "-feature"

scalacOptions += "-language:postfixOps"

scalacOptions += "-deprecation"

scalacOptions += "-target:jvm-1.7"

licenses += "Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html")

homepage := Some(url("https://github.com/gwen-interpreter/gwen"))

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

javaSource in Compile := baseDirectory.value / "src/main/scala"

javaSource in Test := baseDirectory.value / "src/test/scala"

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "io.cucumber" % "gherkin3" % "3.1.2"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.3.9"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2"

libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.7.7"

libraryDependencies += "jline" % "jline" % "2.13"

libraryDependencies += "commons-codec" % "commons-codec" % "1.10"

libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.2.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.mockito" % "mockito-all" % "1.10.19" % "test"

mappings in (Compile, packageBin) ++= Seq(
  file("LICENSE") -> "LICENSE",
  file("NOTICE") -> "NOTICE",
  file("LICENSE-Third-Party.txt") -> "LICENSE-Third-Party.txt",
  file("CHANGELOG") -> "CHANGELOG"
)
