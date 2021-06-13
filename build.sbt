lazy val gwen = (project in file("."))
  .settings(
    projectSettings,
    libraryDependencies ++= mainDependencies ++ testDependencies
  )

lazy val projectSettings = Seq(
  name := "gwen",
  description := "A Given-When-Then interpreter for Gherkin",
  scalaVersion := "3.0.0",
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
    "-Xtarget:8"
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
  val scopt = "4.0.1"
  val slf4jLog4j = "1.7.26"
  val scalaLogging = "3.9.4"
  val jline = "2.14.6"
  val commonCodec = "1.15"
  val commonsText = "1.9"
  val scalaCSV = "1.3.8"
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
    ("com.lihaoyi" %% "scalatags" % scalaTags).cross(CrossVersion.for3Use2_13),
    "net.sourceforge.htmlcleaner" % "htmlcleaner" % htmlCleaner

  )
}

lazy val testDependencies = {
  val scalaTest = "3.2.9"
  val scalaTestPlusMockito = "3.2.9.0"
  val mockitoCore = "3.11.1"
  val h2 = "1.4.200"
  // val slick = "3.3.3"

  Seq(
    "org.scalatest" %% "scalatest" % scalaTest,
    "org.scalatestplus" %% "mockito-3-4" % scalaTestPlusMockito,
    "org.mockito" % "mockito-core" % mockitoCore,
    "com.h2database" % "h2" % h2,
    // TODO: uncomment and re-enable SQLSupportTest once slick releases a Scala 3-compatible version
    // "com.typesafe.slick" %% "slick" % slick
  ).map(_ % Test)
}

Compile / packageBin / mappings ++= Seq(
  file("README.md") -> "README.txt",
  file("LICENSE") -> "LICENSE.txt",
  file("NOTICE") -> "NOTICE.txt",
  file("LICENSE-THIRDPARTY") -> "LICENSE-THIRDPARTY.txt",
  file("CHANGELOG") -> "CHANGELOG.txt"
)
