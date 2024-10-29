/*
 * Copyright 2014-2021 Branko Juric, Brady Wood
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package gwen.core

import gwen.core._
import gwen.core.Errors.UnboundAttributeException
import gwen.core.node.gherkin.Tag
import gwen.core.report.ReportFormat

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.chaining._

import java.io.File
import org.scalatest.matchers.should.Matchers
import gwen.core.init.InitOption

class GwenOptionsTest extends BaseTest with Matchers {

  val rootDir: File = new File("target" + File.separator + "props") tap { _.mkdirs() }
  val singleProcessBaseDir = new File("src/test/resources/singleProcess")
  val multiProcessBaseDir = new File("src/test/resources/multiProcess")
  val noProcessBaseDir = new File("src/test/resources/noProcess")
  val singleProcess = Process("single", singleProcessBaseDir)

  "Options with no command line args" should "parse" in {
    parseOptions(Array[String](), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options)
      }
      case Failure(error) => 
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with existing process option" should "parse" in {
    parseOptions(Array("-p", "single"), singleProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, process = Process("single", singleProcessBaseDir))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--process", "single"), singleProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, process = Process("single", singleProcessBaseDir))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with no process option" should "parse when multiple processes exist" in {
    parseOptions(Array(), multiProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, process = Process("", multiProcessBaseDir))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array(), multiProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, process = Process("", multiProcessBaseDir))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with missing process option" should "not parse" in {
    parseOptions(Array("-p", "missing"), noProcessBaseDir) match {
      case Success(options) => {
        fail("expected failure but was successful")
      }
      case Failure(error) =>
        error.getMessage should be ("At least one of missing.conf, missing.json or missing.properties files must exist in src/test/resources/noProcess/conf/process for missing process")
    }
    parseOptions(Array("--process", "missing"), noProcessBaseDir) match {
      case Success(options) => {
        fail("expected failure but was successful")
      }
      case Failure(error) =>
        error.getMessage should be ("At least one of missing.conf, missing.json or missing.properties files must exist in src/test/resources/noProcess/conf/process for missing process")
    }
  }

  "Options with single process option and feature files" should "not parse" in {
    createDir("dir7")
    val featureFile = createFile("dir7/file.feature")
    parseOptions(Array("-p", singleProcess.name, featureFile.getPath), singleProcessBaseDir) match {
      case Success(options) => {
        fail("expected failure but was successful")
      }
      case Failure(error) =>
        error.getMessage should be ("Cannot specify features on command line when launching single process (use gwen.launch.options.features setting in src/test/resources/singleProcess/conf/process/single.conf file instead)")
    }
    parseOptions(Array("--process", singleProcess.name, featureFile.getPath), singleProcessBaseDir) match {
      case Success(options) => {
        fail("expected failure but was successful")
      }
      case Failure(error) =>
        error.getMessage should be ("Cannot specify features on command line when launching single process (use gwen.launch.options.features setting in src/test/resources/singleProcess/conf/process/single.conf file instead)")
    }
  }

  "Options with single process option and meta files" should "not parse" in {
    createDir("dir8")
    val metaFile = createFile("dir8/file.meta")
    parseOptions(Array("-p", singleProcess.name, "-m", metaFile.getPath), singleProcessBaseDir) match {
      case Success(options) => {
        fail("expected failure but was successful")
      }
      case Failure(error) =>
        error.getMessage should be ("Cannot specify meta on command line when launching single process (use gwen.launch.options.meta setting in src/test/resources/singleProcess/conf/process/single.conf file instead)")
    }
    parseOptions(Array("--process", singleProcess.name, "-m", metaFile.getPath), singleProcessBaseDir) match {
      case Success(options) => {
        fail("expected failure but was successful")
      }
      case Failure(error) =>
        error.getMessage should be ("Cannot specify meta on command line when launching single process (use gwen.launch.options.meta setting in src/test/resources/singleProcess/conf/process/single.conf file instead)")
    }
  }

  "Options with batch option and no files" should "fail" in {
    parseOptions(Array("-b"), noProcessBaseDir) match {
      case Success(options) => {
        fail("expected failure but was successful")
      }
      case Failure(error) =>
        error.getMessage should be ("No feature files or directories provided")
    }
    parseOptions(Array("--batch"), noProcessBaseDir) match {
      case Success(options) => {
        fail("expected failure but was successful")
      }
      case Failure(error) =>
        error.getMessage should be ("No feature files or directories provided")
    }
  }

  "Options with batch option and files " should "parse" in {
    parseOptions(Array("-b", "."), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, batch = true, features = List(new File(".")))

      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--batch", "."), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, batch = true, features = List(new File(".")))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

   "Options with repl and batch option and no files" should "parse" in {
    parseOptions(Array("--repl", "-b"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, repl = true, batch = false)
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--batch", "--repl"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, repl = true, batch = false)
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with repl and batch option and files " should "fail" in {
    parseOptions(Array("-b", "--repl", "."), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, repl = true, batch = false)

      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--repl", "--batch", "."), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, repl = true, batch = false)
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with verbose option and files " should "parse" in {
    parseOptions(Array("-v", "."), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, verbose = true, features = List(new File(".")))

      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--verbose", "."), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, verbose = true, features = List(new File(".")))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with debug option and files " should "parse" in {
    parseOptions(Array("-d", "."), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, debug = true, features = List(new File(".")))

      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--debug", "."), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, debug = true, features = List(new File(".")))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with dry run option and no files" should "be ok" in {
    parseOptions(Array("-n"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, dryRun = true)
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--dry-run"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, dryRun = true)
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with dry run option and files " should "parse" in {
    parseOptions(Array("-n", "."), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, dryRun = true, features = List(new File(".")))

      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--dry-run", "."), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, dryRun = true, features = List(new File(".")))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with parallel option with implied batch mode" should "parse" in {
    parseOptions(Array("--parallel", "."), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, batch = true, parallel = true, features = List(new File(".")))
      }
      case Failure(error) => fail(s"expected options but failed with error: $error")
    }
  }

  "Options with parallel option with explicit batch mode" should "parse" in {
    parseOptions(Array("--parallel", "-b", "."), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, batch = true, parallel = true, features = List(new File(".")))
      }
      case Failure(error) => fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--parallel", "--batch", "."), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, batch = true, parallel = true, features = List(new File(".")))
      }
      case Failure(error) => fail(s"expected options but failed with error: $error")
    }
  }

  "Options with report option but no target report directory" should "not parse" in {
    parseOptions(Array("-r"), noProcessBaseDir) match {
      case Success(options) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
    parseOptions(Array("--report"), noProcessBaseDir) match {
      case Success(options) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
  }

  "Options with report option and report directory" should "parse" in {
    parseOptions(Array("-r", "target/report"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, reportDir = Some(new File("target/report")))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--report", "target/report"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, reportDir = Some(new File("target/report")))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with format option but no report directory" should "get default report dir" in {
    parseOptions(Array("-f", "html"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, reportDir = GwenOptions.Defaults.report, reportFormats=List(ReportFormat.html))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--formats", "html"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, reportDir = GwenOptions.Defaults.report, reportFormats=List(ReportFormat.html))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with format option but no format value(s)" should "not parse" in {
    parseOptions(Array("-f"), noProcessBaseDir) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
    parseOptions(Array("--formats"), noProcessBaseDir) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
  }

  "Options with format options and report directory" should "parse" in {
    parseOptions(Array("-r", "target/report", "-f", "html,junit"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, reportDir = Some(new File("target/report")), reportFormats=List(ReportFormat.html, ReportFormat.junit))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--report", "target/report", "--formats", "html,junit"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, reportDir = Some(new File("target/report")), reportFormats=List(ReportFormat.html, ReportFormat.junit))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with CSV input data file" should "parse" in {
    createFile("data.csv")
    parseOptions(Array("-i", "target/props/data.csv"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, dataFile = Some(new File("target/props/data.csv")))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--input-data", "target/props/data.csv"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, dataFile = Some(new File("target/props/data.csv")))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with non existing CSV input data file" should "error" in {
    parseOptions(Array("-i", "missing.csv"), noProcessBaseDir) match {
      case Success(_) => {
        fail("missing csv file should result in error")
      }
      case Failure(error) =>
    }
  }

  "Options with JSON input data file" should "parse" in {
    createFile("data.json")
    parseOptions(Array("-i", "target/props/data.json"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, dataFile = Some(new File("target/props/data.json")))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--input-data", "target/props/data.json"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, dataFile = Some(new File("target/props/data.json")))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with non existing JSON input data file" should "error" in {
    parseOptions(Array("-i", "missing.json"), noProcessBaseDir) match {
      case Success(_) => {
        fail("missing json file should result in error")
      }
      case Failure(error) =>
    }
  }

  "Options with conf option with no file" should "not parse" in {
    parseOptions(Array("-c"), noProcessBaseDir) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
    parseOptions(Array("--conf"), noProcessBaseDir) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
  }

  "Options with config option with non existing conf file" should "not parse" in {
    parseOptions(Array("-c", "nonexisting.conf"), noProcessBaseDir) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
    parseOptions(Array("--conf", "nonexisting.conf"), noProcessBaseDir) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
  }

  "Options with conf option and existing conf file" should "parse" in {
    val confFile = createFile("gwen.conf")
    parseOptions(Array("-c", confFile.getPath), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, settingsFiles = List(confFile))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--conf", confFile.getPath), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, settingsFiles = List(confFile))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with conf option and multiple existing config files" should "parse" in {
    val configFileA = createFile("gwen-a.json")
    val configFileB = createFile("gwen-b.conf")
    parseOptions(Array("-c", configFileA.getPath + "," + configFileB.getPath), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, settingsFiles = List(configFileA, configFileB))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--conf", configFileA.getPath + "," + configFileB.getPath), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, settingsFiles = List(configFileA, configFileB))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with tags option with no tags" should "not parse" in {
    parseOptions(Array("-t"), noProcessBaseDir) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
    parseOptions(Array("--tags"), noProcessBaseDir) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
  }

  "Options with tags option and valid single include tag" should "parse" in {
    parseOptions(Array("-t", "@wip"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, tags = List((Tag("@wip"), true)))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--tags", "@wip"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, tags = List((Tag("@wip"), true)))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with tags option and valid single exclude tag" should "parse" in {
    parseOptions(Array("-t", "~@wip"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, tags = List((Tag("@wip"), false)))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--tags", "~@wip"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, tags = List((Tag("@wip"), false)))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with tags option and invalid single tag" should "not parse" in {
    parseOptions(Array("-t", "wip"), noProcessBaseDir) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
    parseOptions(Array("--tags", "!wip"), noProcessBaseDir) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
  }

  "Options with tags option and mutiple includes" should "parse" in {
    val tags = "@wip,@regression,@transactional,@simple"
    val expected: List[(Tag, Boolean)] = List((Tag("@wip"), true), (Tag("@regression"), true), (Tag("@transactional"), true), (Tag("@simple"), true))
    parseOptions(Array("-t", tags), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, tags = expected)
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--tags", tags), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, tags = expected)
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with tags option and mutiple exclude tags" should "parse" in {
    val tags = "~@experimental,~@complex"
    val expected: List[(Tag, Boolean)] = List((Tag("@experimental"), false), (Tag("@complex"), false))
    parseOptions(Array("-t", tags), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, tags = expected)
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--tags", tags), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, tags = expected)
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with tags option and mutiple include and exclude tags" should "parse" in {
    val tags = "@wip,@regression,~@experimental,@transactional,~@complex,@simple"
    val expected: List[(Tag, Boolean)] = List((Tag("@wip"), true), (Tag("@regression"), true), (Tag("@experimental"), false), (Tag("@transactional"), true), (Tag("@complex"), false), (Tag("@simple"), true))
    parseOptions(Array("-t", tags), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, tags = expected)
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--tags", tags), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, tags = expected)
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with tags option and two tags separated by space" should "not parse" in {
    val tags = "@wip @regression"
    parseOptions(Array("-t", tags), noProcessBaseDir) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
    parseOptions(Array("--tags", tags), noProcessBaseDir) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
  }

  "Options with tags option and one valid tag and one invalid tag" should "not parse" in {
    val tags = "@valid,invalid"
    parseOptions(Array("-t", tags), noProcessBaseDir) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
    parseOptions(Array("--tags", tags), noProcessBaseDir) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
  }

  "Options with meta option with no meta file" should "not parse" in {
    parseOptions(Array("-m"), noProcessBaseDir) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
    parseOptions(Array("--meta"), noProcessBaseDir) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
  }

  "Options with with non existing meta file" should "not parse" in {
    parseOptions(Array("-m", "nonexisting.meta"), noProcessBaseDir) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
    parseOptions(Array("--meta", "nonexisting.meta"), noProcessBaseDir) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
  }

  "Options with one existing meta file" should "parse" in {
    val metaFile = createFile("gwen.meta")
    parseOptions(Array("-m", metaFile.getPath), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, metaFiles = List(metaFile))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--meta", metaFile.getPath), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, metaFiles = List(metaFile))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with multiple existing meta files" should "parse" in {
    val metaFile1 = createFile("gwen1.meta")
    val metaFile2 = createFile("gwen2.meta")
    val metaFiles = List(metaFile1, metaFile2)
    val metaPaths = metaFiles.map(_.getPath).mkString(",")
    parseOptions(Array("-m", metaPaths), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, metaFiles = metaFiles)
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--meta", metaPaths), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, metaFiles = metaFiles)
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with multiple existing and one missing meta file" should "not parse" in {
    val metaFile1 = createFile("gwen1.meta")
    val metaFile2 = new File("gwen2.meta")
    val metaFile3 = createFile("gwen3.meta")
    val metaFiles = List(metaFile1, metaFile2, metaFile3)
    val metaPaths = metaFiles.map(_.getPath).mkString(",")
    parseOptions(Array("-m", metaPaths), noProcessBaseDir) match {
      case Success(files) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
    parseOptions(Array("--meta", metaPaths), noProcessBaseDir) match {
      case Success(files) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
  }

  "Options with meta multiple meta files" should "parse" in {
    val metaFile1 = createFile("gwen1.meta")
    val metaFile2 = createFile("gwen2.meta")
    val metaFiles = List(metaFile1, metaFile2)
    val metaPaths = metaFiles.map(_.getPath).mkString(",")
    parseOptions(Array("-m", metaPaths), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, metaFiles = metaFiles)
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
    parseOptions(Array("--meta", metaPaths), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, metaFiles = metaFiles)
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with existing feature file path" should "parse" in {

    createDir("dir1")
    val feature1 = createFile("dir1/file1.feature")

    parseOptions(Array(feature1.getPath), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, features = List(feature1))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with existing directory path" should "parse" in {

    val dir2 = createDir("dir2")

    parseOptions(Array(dir2.getPath), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, features = List(dir2))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }

  }

  "Options with existing paths" should "parse" in {

    val dir3 = createDir("dir3")
    val feature3 = createFile("dir3/file3.feature")
    val dir4 = createDir("dir4")

    parseOptions(Array(dir3.getPath, feature3.getPath, dir4.getPath), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, features = List(dir3, feature3, dir4))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with nonexisting paths" should "not parse" in {

    parseOptions(Array("nonexistindir", "nonexisting.file"), noProcessBaseDir) match {
      case Success(options) => {
        fail("expected None but got options")
      }
      case Failure(error) =>
    }
  }

  "Options with all valid options" should "parse" in {

    val reportDir = new File("target/report")
    val confFile = createFile("gwen-1.conf")
    val dataFile = createFile("gwen-1.csv")
    val tags = "@wip,@regression,~@experimental,@transactional,~@complex,@simple"
    val metaFile = createFile("gwen.meta")
    val dir5 = createDir("dir5")
    val feature5 = createFile("dir5/file5.feature")
    val dir6 = createDir("dir6")

    parseOptions(Array("-b", "--parallel", "-v", "-r", reportDir.getPath, "-f", "html,junit", "-c", confFile.getPath, "-t", tags, "-i", dataFile.getPath, "-m", metaFile.getPath, dir5.getPath, feature5.getPath, dir6.getPath), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(
          options,
          process = Process("", noProcessBaseDir),
          repl = false,
          batch = true,
          parallel = true,
          verbose = true,
          debug = false,
          Some(reportDir),
          List(ReportFormat.html, ReportFormat.junit),
          List(confFile),
          List((Tag("@wip"), true), (Tag("@regression"), true), (Tag("@experimental"), false), (Tag("@transactional"), true), (Tag("@complex"), false), (Tag("@simple"), true)),
          dryRun = false,
          Some(dataFile),
          List(metaFile),
          List(dir5, feature5, dir6),
          pretty = false)
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }

    parseOptions(Array("--batch", "--parallel", "--verbose", "--report", reportDir.getPath(), "--formats", "html,junit", "--conf", confFile.getPath(), "--tags", tags, "--input-data", dataFile.getPath(), "--meta", metaFile.getPath(), dir5.getPath(), feature5.getPath(), dir6.getPath), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(
          options,
          process = Process("", noProcessBaseDir),
          repl = false,
          batch = true,
          parallel = true,
          verbose = true,
          debug = false,
          Some(reportDir),
          List(ReportFormat.html, ReportFormat.junit),
          List(confFile),
          List((Tag("@wip"), true), (Tag("@regression"), true), (Tag("@experimental"), false), (Tag("@transactional"), true), (Tag("@complex"), false), (Tag("@simple"), true)),
          dryRun = false,
          Some(dataFile),
          List(metaFile),
          List(dir5, feature5, dir6),
          pretty = false)
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }

  }

  "Options with init command" should "parse" in {

    parseOptions(Array("init"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, init = true, initDir = new File("gwen"))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with init command and non existing dir" should "parse" in {

    parseOptions(Array("init", "gwen"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, init = true, initDir = new File("gwen"))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with init --docker command" should "parse" in {

    parseOptions(Array("init", "--docker"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, init = true, docker = true, initDir = new File("gwen"))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with init --jenkins command" should "parse" in {

    parseOptions(Array("init", "--jenkins"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, init = true, jenkins = true, initDir = new File("gwen"))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with init --jenkins and --force command" should "parse" in {

    parseOptions(Array("init", "--jenkins", "--force"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, init = true, jenkins = true, force = true, initDir = new File("gwen"))
      }
      case Failure(error) =>
        fail(s"expected options but failed with error: $error")
    }
  }

  "Options with pretty format command without switch or dir" should "fail" in {
    parseOptions(Array("format"), noProcessBaseDir) match {
      case Success(options) => {
        fail("expected failure but was successful")
      }
      case Failure(e) => 
        e.getMessage should be ("Gwen invocation failed (see log for details)")
    }
  }

  "Options with pretty format command without dir" should "parse" in {
    parseOptions(Array("format", "--pretty"), noProcessBaseDir) match {
      case Success(options) => {
        fail("expected failure but was successful")
      }
      case Failure(e) => 
        e.getMessage should be ("Gwen invocation failed (see log for details)")
    }
  }

  "Options with pretty format command with dir" should "parse" in {
    parseOptions(Array("format", "--pretty", "src/test/features"), noProcessBaseDir) match {
      case Success(options) => {
        assertOptions(options, pretty = true, formatFiles = List(new File("src/test/features")))
      }
      case Failure(error) => fail(s"expected options but failed with error: $error")
    }
  }

  private def parseOptions(args: Array[String], baseDir: File): Try[GwenOptions] = Try {
    GwenOptions(args, baseDir)
  }

  private def assertOptions(
                             options: GwenOptions,
                             process: Process = GwenOptions.Defaults.process,
                             repl: Boolean = GwenOptions.Defaults.repl,
                             batch: Boolean = GwenOptions.Defaults.batch,
                             parallel: Boolean = GwenOptions.Defaults.parallel,
                             verbose: Boolean = GwenOptions.Defaults.verbose,
                             debug: Boolean = GwenOptions.Defaults.debug,
                             reportDir: Option[File] = GwenOptions.Defaults.report,
                             reportFormats: List[ReportFormat] = GwenOptions.Defaults.format,
                             settingsFiles: List[File] = GwenOptions.Defaults.conf,
                             tags: List[(Tag, Boolean)] = GwenOptions.Defaults.tags,
                             dryRun: Boolean = GwenOptions.Defaults.dryRun,
                             dataFile: Option[File] = GwenOptions.Defaults.inputData,
                             metaFiles: List[File] = GwenOptions.Defaults.meta,
                             features: List[File] = GwenOptions.Defaults.features,
                             init: Boolean = false,
                             docker: Boolean = false,
                             jenkins: Boolean = false,
                             force: Boolean = false,
                             initDir: File = GwenOptions.Defaults.initDir,
                             pretty: Boolean = GwenOptions.Defaults.pretty,
                             formatFiles: List[File] = Nil): Unit = {

    options.process.name should be (process.name)
    options.process.settingsFile.map(_.getCanonicalPath) should be (process.settingsFile.map(_.getCanonicalPath))
    options.batch should be (batch && !repl)
    options.repl should be (repl)
    options.parallel should be (parallel)
    options.verbose should be (verbose)
    options.debug should be (debug)
    options.reportDir should be (reportDir)
    options.reportFormats should be (reportFormats)
    options.settingsFiles should be (settingsFiles)
    options.tags should be (tags)
    options.dryRun should be (dryRun)
    options.dataFile should be (dataFile)
    options.metas should be (FileIO.appendFile(metaFiles, Settings.UserMeta))
    options.features should be (features)
    options.init should be (init)
    val expectedInitOptions = List(
      if (docker) Some(InitOption.docker) else None,
      if (jenkins) Some(InitOption.jenkins) else None,
      if (force) Some(InitOption.force) else None
    ).flatten
    
    if (docker) {
      options.initOptions should contain (InitOption.docker)
    } else {
      options.initOptions should not contain (InitOption.docker)
    }
    if (jenkins) {
      options.initOptions should contain (InitOption.jenkins)
    } else {
      options.initOptions should not contain (InitOption.jenkins)
    }
    if (force) {
      options.initOptions should contain (InitOption.force)
    } else {
      options.initOptions should not contain (InitOption.force)
    }
    options.initDir should be (initDir)
    options.pretty should be (pretty)
    options.formatFiles should be (formatFiles)

    options.interpolate("batch is $<gwen.options.batch>, yep") should be (s"batch is $batch, yep")
    options.interpolate("parallel is $<gwen.options.parallel>, yep") should be (s"parallel is $parallel, yep")
    options.interpolate("verbose is $<gwen.options.verbose>, yep") should be (s"verbose is $verbose, yep")
    options.interpolate("debug is $<gwen.options.debug>, yep") should be (s"debug is $debug, yep")
    options.interpolate("reportDir is $<gwen.options.reportDir>, yep") should be (s"reportDir is ${reportDir.getOrElse("")}, yep")
    options.interpolate("reportFormats is $<gwen.options.reportFormats>, yep") should be (s"reportFormats is ${reportFormats.mkString(",")}, yep")
    options.interpolate("settingsFiles is $<gwen.options.settingsFiles>, yep") should be (s"settingsFiles is ${settingsFiles.mkString(",")}, yep")
    options.interpolate("tags is $<gwen.options.tags>, yep") should be (s"tags is ${tags.map((t, include) => s"${if (include) "" else "~"}$t").mkString(",")}, yep")
    options.interpolate("dryRun is $<gwen.options.dryRun>, yep") should be (s"dryRun is $dryRun, yep")
    options.interpolate("dataFile is $<gwen.options.dataFile>, yep") should be (s"dataFile is ${dataFile.getOrElse("")}, yep")
    options.interpolate("metas is $<gwen.options.metas>, yep") should be (s"metas is ${FileIO.appendFile(metaFiles, Settings.UserMeta).mkString(",")}, yep")
    options.interpolate("features is $<gwen.options.features>, yep") should be (s"features is ${features.mkString(" ")}, yep")
    options.interpolate("init is $<gwen.options.init>, yep") should be (s"init is $init, yep")
    options.interpolate("initOptions is $<gwen.options.initOptions>, yep") should be (s"initOptions is ${expectedInitOptions.map(opt => s"--$opt").sorted.mkString(" ")}, yep")
    options.interpolate("initDir is $<gwen.options.initDir>, yep") should be (s"initDir is ${Option(initDir).getOrElse("")}, yep")
    options.interpolate("pretty is $<gwen.options.pretty>, yep") should be (s"pretty is $pretty, yep")
    options.interpolate("formatFiles is $<gwen.options.formatFiles>, yep") should be (s"formatFiles is ${formatFiles.mkString(" ")}, yep")

  }

  "Interpolation of unkown options placeholder" should "result in unbound ref error" in {
    intercept[UnboundAttributeException] {
      new GwenOptions().interpolate("unkown is $<gwen.options.unkown>, yep")
    }
  }

  "Interpolation of non options placeholder" should "have no effect" in {
    new GwenOptions().interpolate("someting else is $<something.else>, yep") should be ("someting else is $<something.else>, yep")
  }

  private def createFile(filepath: String): File = {
    val file = new File(rootDir.getPath + File.separator + filepath.replace('/', File.separatorChar))
    if (file.exists) {
      file.delete()
    }
    file.getParentFile.mkdirs()
    file.createNewFile()
    file
  }

  private def createDir(dirname: String): File = {
    val dir = new File(rootDir, dirname)
    dir.deleteDir()
    dir.mkdirs()
    dir
  }



}
