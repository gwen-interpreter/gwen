enablePlugins(GitVersioning)

// gwen core version
val gwenVersion = "3.45.0"

git.baseVersion := gwenVersion
git.useGitDescribe := true

lazy val gwen = (project in file("."))
  .settings(
    projectSettings,
    libraryDependencies ++= mainDependencies ++ testDependencies
  )

lazy val projectSettings = Seq(
  name := "gwen",
  description := "Core Gwen interpreter",
  scalaVersion := "3.3.0",
  organization := "org.gweninterpreter",
  homepage := Some(url("https://gweninterpreter.org")),
  organizationHomepage := Some(url("https://github.com/gwen-interpreter")),
  startYear := Some(2014),
  licenses += "Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html"),
  versionScheme := Some("semver-spec"),
  trapExit := false,
  crossPaths := false,
  scalacOptions ++= Seq(
    "-feature",
    "-language:postfixOps",
    "-deprecation"
  ),
  initialize := {
    val _ = initialize.value
    val javaVersion = sys.props("java.specification.version")
    if (javaVersion != "11")
      sys.error(s"JDK 11 is required to build this project. Found $javaVersion instead")
  }
)

lazy val mainDependencies = {
  val cucumberGherkin = "26.2.0"
  val scopt = "4.1.0"
  val scalaLogging = "3.9.5"
  val jline = "3.23.0"
  val slf4j = "1.7.36"
  val slf4jLog4j = "2.20.0"
  val commonCodec = "1.16.0"
  val commonsText = "1.10.0"
  val scalaCSV = "1.3.10"
  val jsonPath = "2.8.0"
  val jodaTime = "2.12.5"
  val scalaTags = "0.12.0"
  val htmlCleaner = "2.29"
  val rpCommon = "5.8.1"
  val rpClientJava = "5.1.22"
  val tsConfig = "1.4.2"
  val jansi = "2.4.0"
  val pdfBox = "3.0.0"

  Seq(
    "io.cucumber" % "gherkin" % cucumberGherkin,
    "com.github.scopt" %% "scopt" % scopt,
    "com.typesafe.scala-logging" %% "scala-logging" % scalaLogging,
    "org.jline" % "jline" % jline,
    "org.apache.logging.log4j" % "log4j-slf4j-impl" % slf4jLog4j,
    "org.slf4j" % "jul-to-slf4j" % slf4j,
    "commons-codec" % "commons-codec" % commonCodec,
    "org.apache.commons" % "commons-text" % commonsText,
    "com.github.tototoshi" %% "scala-csv" % scalaCSV,
    "com.jayway.jsonpath" % "json-path" % jsonPath,
    "joda-time" % "joda-time" % jodaTime,
    "com.lihaoyi" %% "scalatags" % scalaTags,
    "net.sourceforge.htmlcleaner" % "htmlcleaner" % htmlCleaner,
    "com.typesafe" % "config" % tsConfig,
    "com.epam.reportportal" % "commons-model" % rpCommon,
    "com.epam.reportportal" % "client-java" % rpClientJava excludeAll(
      ExclusionRule(organization = "org.aspectj", name = "aspectjrt"),
      ExclusionRule(organization = "org.aspectj", name = "aspectjweaver"),
      ExclusionRule(organization = "org.slf4j", name = "slf4j-api")
    ),
    "org.fusesource.jansi" % "jansi" % jansi,
    "org.apache.pdfbox" % "pdfbox" % pdfBox,
    "org.apache.pdfbox" % "pdfbox-io" % pdfBox
  ) ++ mainOverrides
}

lazy val mainOverrides = {
  val jacksonDataBind = "2.15.2"
  val guava = "32.0.1-jre"
  Seq(
    "com.fasterxml.jackson.core" %  "jackson-databind" % jacksonDataBind,
    "com.google.guava" % "guava" % guava
  )
}

lazy val testDependencies = {
  val scalaTest = "3.2.16"
  val scalaTestPlusMockito = "3.2.11.0"
  val mockitoCore = "4.11.0"
  val h2 = "2.2.220"
  // val slick = "3.3.3"

  Seq(
    "org.scalatest" %% "scalatest" % scalaTest,
    "org.scalatestplus" %% "mockito-4-2" % scalaTestPlusMockito,
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

Test/ parallelExecution := false
