enablePlugins(GitVersioning)

// gwen core version
val gwenVersion = "3.53.2"

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
  scalaVersion := "3.4.0",
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

val slf4j = ""

lazy val mainDependencies = {
  Seq(
    "io.cucumber" % "gherkin" % "28.0.0",
    "com.github.scopt" %% "scopt" % "4.1.0",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
    "org.jline" % "jline" % "3.25.1",
    "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.23.0",
    "org.slf4j" % "jul-to-slf4j" % "1.7.36",
    "commons-codec" % "commons-codec" % "1.16.1",
    "org.apache.commons" % "commons-text" % "1.11.0",
    "com.github.tototoshi" %% "scala-csv" % "1.3.10",
    "com.jayway.jsonpath" % "json-path" % "2.9.0",
    "com.lihaoyi" %% "scalatags" % "0.12.0",
    "net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.29",
    "com.typesafe" % "config" % "1.4.3",
    "com.epam.reportportal" % "commons-model" % "5.11.1",
    "com.epam.reportportal" % "client-java" % "5.2.5" excludeAll(
      ExclusionRule(organization = "org.aspectj"),
      ExclusionRule(organization = "org.jetbrains.kotlin")
    ),
    "org.fusesource.jansi" % "jansi" % "2.4.1",
    "org.apache.pdfbox" % "pdfbox" % "3.0.1" excludeAll(
      ExclusionRule(organization = "org.junit.jupiter")
    ),
    "org.apache.pdfbox" % "pdfbox-io" % "3.0.1" excludeAll(
      ExclusionRule(organization = "org.junit.jupiter")
    )
  ) ++ mainOverrides
}

lazy val mainOverrides = {
  Seq(
    "com.fasterxml.jackson.core" %  "jackson-databind" % "2.16.1",
    "com.google.guava" % "guava" % "33.0.0-jre",
    "org.reactivestreams" % "reactive-streams" % "1.0.4",
    "org.slf4j" % "slf4j-api" % "1.7.36"
  )
}

dependencyOverrides ++= Seq(
  "com.fasterxml.jackson.core" %  "jackson-databind" % "2.16.1",
  "com.google.guava" % "guava" % "33.0.0-jre",
  "org.reactivestreams" % "reactive-streams" % "1.0.4",
  "org.slf4j" % "slf4j-api" % "1.7.36"
)

lazy val testDependencies = {
  Seq(
    "org.scalatest" %% "scalatest" % "3.2.18",
    "org.scalatestplus" %% "mockito-4-2" % "3.2.11.0",
    "org.mockito" % "mockito-core" % "4.11.0"
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
