import com.typesafe.sbt.SbtGit._
import sbt.Keys.{crossPaths, description, startYear}

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

lazy val commonSettings = Seq(
  name := "gwen",
  description := "A Given-When-Then interpreter and automation platform",
  scalaVersion := "2.12.6",
  organization := "org.gweninterpreter",
  homepage := Some(url("https://github.com/gwen-interpreter/gwen")),
  organizationHomepage := Some(url("http://gweninterpreter.org")),
  startYear := Some(2014),
  licenses += "Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html"),
  trapExit := false,
  crossPaths := false,
  scalacOptions ++= Seq(
    "-feature",
    "-language:postfixOps",
    "-deprecation",
    "-target:jvm-1.8"
  )
)

lazy val testDependencies = {
  val scalaTest = "3.0.5"
  val mockitoAll = "1.10.19"
  val h2 = "1.4.197"
  val slick = "3.2.3"

  Seq(
    "org.scalatest" %% "scalatest" % scalaTest,
    "org.mockito" % "mockito-all" % mockitoAll,
    "com.h2database" % "h2" % h2,
    "com.typesafe.slick" %% "slick" % slick
  ).map(_ % Test)
}

lazy val commonDependencies = {
  val cucumberGherkin = "5.1.0"
  val scopt = "3.7.0"
  val slf4jLog4j = "1.7.25"
  val scalaLogging = "3.9.0"
  val jline = "2.14.6"
  val commonCodec = "1.11"
  val commonsText = "1.4"
  val scalaCSV = "1.3.5"
  val jsonPath = "2.4.0"
  val jodaTime = "2.10"

  Seq(
    "io.cucumber" % "gherkin" % cucumberGherkin,
    "com.github.scopt" %% "scopt" % scopt,
    "org.slf4j" % "slf4j-log4j12" % slf4jLog4j,
    "com.typesafe.scala-logging" %% "scala-logging" % scalaLogging,
    "jline" % "jline" % jline,
    "commons-codec" % "commons-codec" % commonCodec,
    "org.apache.commons" % "commons-text" % commonsText,
    "com.github.tototoshi" %% "scala-csv" % scalaCSV,
    "com.jayway.jsonpath" % "json-path" % jsonPath,
    "joda-time" % "joda-time" % jodaTime
  )
}

lazy val root = (project in file("."))
  .settings(
    commonSettings,
    libraryDependencies ++= commonDependencies ++ testDependencies
  )

mappings in(Compile, packageBin) ++= Seq(
  file("README.md") -> "README.txt",
  file("LICENSE") -> "LICENSE.txt",
  file("NOTICE") -> "NOTICE.txt",
  file("LICENSE-THIRDPARTY") -> "LICENSE-THIRDPARTY.txt",
  file("CHANGELOG") -> "CHANGELOG.txt"
)
