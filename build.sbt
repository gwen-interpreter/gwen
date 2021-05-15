lazy val gwen = (project in file("."))
  .settings(
    projectSettings,
    libraryDependencies ++= mainDependencies ++ testDependencies
  )
  
lazy val projectSettings = Seq(
  name := "gwen",
  description := "A Given-When-Then interpreter for Gherkin",
  scalaVersion := "2.13.3",
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
    "-target:8",
    "-Xlint:_,-missing-interpolator"
  ),
  initialize := {
    val _ = initialize.value
    val javaVersion = sys.props("java.specification.version")
    if (javaVersion != "1.8")
      sys.error(s"JDK 8 is required to build this project. Found $javaVersion instead")
  }
)

lazy val mainDependencies = {
  val cucumberGherkin = "15.0.2"
  val scopt = "3.7.1"
  val slf4jLog4j = "1.7.26"
  val scalaLogging = "3.9.2"
  val jline = "2.14.6"
  val commonCodec = "1.15"
  val commonsText = "1.9"
  val scalaCSV = "1.3.6"
  val jsonPath = "2.4.0"
  val jodaTime = "2.10.6"
  val scalaTags = "0.9.4"
  val htmlCleaner = "2.24"

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
    "joda-time" % "joda-time" % jodaTime,
    "com.lihaoyi" %% "scalatags" % scalaTags,
    "net.sourceforge.htmlcleaner" % "htmlcleaner" % htmlCleaner
    
  )
}

lazy val testDependencies = {
  val scalaTest = "3.0.9"
  val mockitoAll = "1.10.19"
  val h2 = "1.4.200"
  val slick = "3.3.3"

  Seq(
    "org.scalatest" %% "scalatest" % scalaTest,
    "org.mockito" % "mockito-all" % mockitoAll,
    "com.h2database" % "h2" % h2,
    "com.typesafe.slick" %% "slick" % slick
  ).map(_ % Test)
}

mappings in(Compile, packageBin) ++= Seq(
  file("README.md") -> "README.txt",
  file("LICENSE") -> "LICENSE.txt",
  file("NOTICE") -> "NOTICE.txt",
  file("LICENSE-THIRDPARTY") -> "LICENSE-THIRDPARTY.txt",
  file("CHANGELOG") -> "CHANGELOG.txt"
)
