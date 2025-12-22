enablePlugins(GitVersioning)

git.baseVersion := "4.11.1"
git.useGitDescribe := true

lazy val gwen = (project in file("."))
  .settings(
    projectSettings,
    libraryDependencies ++= mainDependencies ++ testDependencies
  )

lazy val projectSettings = Seq(
  name := "gwen",
  description := "Core Gwen interpreter",
  scalaVersion := "3.7.4",
  organization := "org.gweninterpreter",
  homepage := Some(url("https://gweninterpreter.org")),
  organizationHomepage := Some(url("https://github.com/gwen-interpreter")),
  startYear := Some(2014),
  licenses += "Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"),
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
    "io.cucumber" % "gherkin" % "37.0.0",
    "com.github.scopt" %% "scopt" % "4.1.0",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
    "org.jline" % "jline" % "3.30.6",
    "org.slf4j" % "jul-to-slf4j" % "2.0.17",
    "ch.qos.logback" % "logback-core" % "1.5.22",
    "ch.qos.logback" % "logback-classic" % "1.5.22",
    "commons-codec" % "commons-codec" % "1.20.0",
    "org.apache.commons" % "commons-text" % "1.15.0",
    "com.github.tototoshi" %% "scala-csv" % "2.0.0",
    "com.jayway.jsonpath" % "json-path" % "2.10.0",
    "com.lihaoyi" %% "scalatags" % "0.13.1",
    "net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.29",
    "com.typesafe" % "config" % "1.4.3",
    "org.fusesource.jansi" % "jansi" % "2.4.2",
    "com.fasterxml.jackson.core" %  "jackson-databind" % "2.20.1",
    "org.apache.pdfbox" % "pdfbox" % "3.0.6" excludeAll(
      ExclusionRule(organization = "org.junit.jupiter")
    ),
    "org.apache.pdfbox" % "pdfbox-io" % "3.0.6" excludeAll(
      ExclusionRule(organization = "org.junit.jupiter")
    )
  ) ++ mainOverrides
}

lazy val mainOverrides = {
  Seq(
  )
}

lazy val testDependencies = {
  Seq(
    "org.scalatest" %% "scalatest" % "3.2.19",
    "org.scalatestplus" %% "mockito-5-12" % "3.2.19.0",
    "org.mockito" % "mockito-core" % "5.20.0",
    "org.graalvm.js" % "js-scriptengine" % "25.0.0",
    "org.graalvm.js" % "js" % "25.0.0"
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
