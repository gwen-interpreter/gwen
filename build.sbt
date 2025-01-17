enablePlugins(GitVersioning)

// gwen core version
val gwenVersion = "4.2.1"

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
  scalaVersion := "3.6.2",
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
    if (javaVersion != "17")
      sys.error(s"Java 17 is required to build this project. Found $javaVersion instead")
  }
)

val slf4j = ""

lazy val mainDependencies = {
  Seq(
    "io.cucumber" % "gherkin" % "30.0.4",
    "com.github.scopt" %% "scopt" % "4.1.0",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
    "org.jline" % "jline" % "3.28.0",
    "org.slf4j" % "jul-to-slf4j" % "2.0.16",
    "ch.qos.logback" % "logback-core" % "1.5.15",
    "ch.qos.logback" % "logback-classic" % "1.5.15",
    "commons-codec" % "commons-codec" % "1.17.1",
    "org.apache.commons" % "commons-text" % "1.13.0",
    "com.github.tototoshi" %% "scala-csv" % "2.0.0",
    "com.jayway.jsonpath" % "json-path" % "2.9.0",
    "com.lihaoyi" %% "scalatags" % "0.13.1",
    "net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.29",
    "com.typesafe" % "config" % "1.4.3",
    "org.fusesource.jansi" % "jansi" % "2.4.1",
    "com.fasterxml.jackson.core" %  "jackson-databind" % "2.18.2",
    "org.apache.pdfbox" % "pdfbox" % "3.0.3" excludeAll(
      ExclusionRule(organization = "org.junit.jupiter")
    ),
    "org.apache.pdfbox" % "pdfbox-io" % "3.0.3" excludeAll(
      ExclusionRule(organization = "org.junit.jupiter")
    )
  )
}

lazy val testDependencies = {
  Seq(
    "org.scalatest" %% "scalatest" % "3.2.19",
    "org.scalatestplus" %% "mockito-4-5" % "3.2.12.0",
    "org.mockito" % "mockito-core" % "5.14.2",
    "org.graalvm.js" % "js-scriptengine" % "24.1.1",
    "org.graalvm.js" % "js" % "24.1.1",

  ).map(_ % Test)
}

Compile / packageBin / mappings ++= Seq(
  file("README.md") -> "README.txt",
  file("LICENSE") -> "LICENSE.txt",
  file("NOTICE") -> "NOTICE.txt",
  file("LICENSE-THIRDPARTY") -> "LICENSE-THIRDPARTY.txt",
  file("CHANGELOG") -> "CHANGELOG.txt"
)

Test / parallelExecution := false

Test / testOptions += Tests.Argument("-oF")
