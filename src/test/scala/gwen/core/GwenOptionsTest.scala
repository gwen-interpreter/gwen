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
import gwen.core.model.node.Tag
import gwen.core.report.ReportFormat

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.File

class GwenOptionsTest extends FlatSpec with Matchers {
  
  val rootDir: File = new File("target" + File.separator + "props") tap { _.mkdirs() }

  "Options with no command line args" should "parse" in {
    parseOptions(Array[String]()) match {
      case Success(options) => { 
        assertOptions(options)
      }
      case _ => fail("expected options but failed")
    }
  }
  
  "Options with batch option and no files" should "fail" in {
    parseOptions(Array("-b")) match {
      case Success(options) => { 
        fail("expected failure but was successful")
      }
      case Failure(error) => 
        error.getMessage should be ("No feature files or directories specified")
    }
    parseOptions(Array("--batch")) match {
      case Success(options) => { 
        fail("expected failure but was successful")
      }
      case Failure(error) => 
        error.getMessage should be ("No feature files or directories specified")
    }
  }
  
  "Options with batch option and files " should "parse" in {
    parseOptions(Array("-b", ".")) match {
      case Success(options) => { 
        assertOptions(options, batch = true, features = List(new File(".")))

      }
      case _ => 
        fail("expected options but failed")
    }
    parseOptions(Array("--batch", ".")) match {
      case Success(options) => { 
        assertOptions(options, batch = true, features = List(new File(".")))
      }
      case _ => 
        fail("expected options but failed")
    }
  }
  
 "Options with dry run option and no files" should "be ok" in {
    parseOptions(Array("-n")) match {
      case Success(options) => {
        assertOptions(options, dryRun = true)
      }
      case _ =>
        fail("expected options but failed")
    }
    parseOptions(Array("--dry-run")) match {
      case Success(options) => { 
        assertOptions(options, dryRun = true)
      }
      case _ =>
        fail("expected options but failed")
    }
  }
  
  "Options with dry run option and files " should "parse" in {
    parseOptions(Array("-n", ".")) match {
      case Success(options) => { 
        assertOptions(options, dryRun = true, features = List(new File(".")))

      }
      case _ => 
        fail("expected options but failed")
    }
    parseOptions(Array("--dry-run", ".")) match {
      case Success(options) => { 
        assertOptions(options, dryRun = true, features = List(new File(".")))
      }
      case _ => 
        fail("expected options but failed")
    }
  } 
  
  "Options with parallel option with implied batch mode" should "parse" in {
    parseOptions(Array("--parallel", ".")) match {
      case Success(options) => { 
        assertOptions(options, batch = true, parallel = true, features = List(new File(".")))
      }
      case _ => fail("expected options but failed")
    }
  }
  
  "Options with parallel option with explicit batch mode" should "parse" in {
    parseOptions(Array("--parallel", "-b", ".")) match {
      case Success(options) => { 
        assertOptions(options, batch = true, parallel = true, features = List(new File(".")))
      }
      case _ => fail("expected options but failed")
    }
    parseOptions(Array("--parallel", "--batch", ".")) match {
      case Success(options) => { 
        assertOptions(options, batch = true, parallel = true, features = List(new File(".")))
      }
      case _ => fail("expected options but failed")
    }
  }
  
  "Options with report option but no target report directory" should "not parse" in {
    parseOptions(Array("-r")) match {
      case Success(options) => {
        fail("expected None but got options")
      }
      case _ => 
    }
    parseOptions(Array("--report")) match {
      case Success(options) => {
        fail("expected None but got options")
      }
      case _ => 
    }
  }
  
  "Options with report option and report directory" should "parse" in {
    parseOptions(Array("-r", "target/report")) match {
      case Success(options) => {
        assertOptions(options, reportDir = Some(new File("target/report")), reportFormats = List(ReportFormat.html))
      }
      case _ =>
        fail("expected options but failed")
    }
    parseOptions(Array("--report", "target/report")) match {
      case Success(options) => {
        assertOptions(options, reportDir = Some(new File("target/report")), reportFormats = List(ReportFormat.html))
      }
      case _ =>
        fail("expected options but failed")
    }
  }
  
  "Options with format option but no report directory" should "not parse" in {
    parseOptions(Array("-f", "html")) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case _ => 
    }
    parseOptions(Array("--format", "html")) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case _ => 
    }
  }
  
  "Options with format option but no format value(s)" should "not parse" in {
    parseOptions(Array("-f")) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case _ => 
    }
    parseOptions(Array("--format")) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case _ => 
    }
  }
  
  "Options with format options and report directory" should "parse" in {
    parseOptions(Array("-r", "target/report", "-f", "html,junit")) match {
      case Success(options) => {
        assertOptions(options, reportDir = Some(new File("target/report")), reportFormats=List(ReportFormat.html, ReportFormat.junit))
      }
      case _ =>
        fail("expected options but failed")
    }
    parseOptions(Array("--report", "target/report", "--formats", "html,junit")) match {
      case Success(options) => {
        assertOptions(options, reportDir = Some(new File("target/report")), reportFormats=List(ReportFormat.html, ReportFormat.junit))
      }
      case _ =>
        fail("expected options but failed")
    }
  }
  
  "Options with CSV input data file" should "parse" in {
    createFile("data.csv")
    parseOptions(Array("-i", "target/props/data.csv")) match {
      case Success(options) => {
        assertOptions(options, dataFile = Some(new File("target/props/data.csv")))
      }
      case _ =>
        fail("expected options but failed")
    }
    parseOptions(Array("--input-data", "target/props/data.csv")) match {
      case Success(options) => {
        assertOptions(options, dataFile = Some(new File("target/props/data.csv")))
      }
      case _ =>
        fail("expected options but failed")
    }
  }
  
  "Options with non existing CSV input data file" should "error" in {
    parseOptions(Array("-i", "missing.csv")) match {
      case Success(_) => {
        fail("missing csv file should result in error")
      }
      case _ =>
    }
  }
  
  "Options with properties option with no properties file" should "not parse" in {
     parseOptions(Array("-p")) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case _ => 
    }
    parseOptions(Array("--properties")) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case _ => 
    }
  }
  
  "Options with properties option with non existing properties file" should "not parse" in {
     parseOptions(Array("-p", "nonexisting.properties")) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case _ => 
    }
    parseOptions(Array("--properties", "nonexisting.properties")) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case _ => 
    }
  }
  
  "Options with properties option and existing properties file" should "parse" in {
    val propsFile = createFile("gwen.properties")
    parseOptions(Array("-p", propsFile.getPath)) match {
      case Success(options) => {
        assertOptions(options, properties = List(propsFile))
      }
      case _ =>
        fail("expected options but failed")
    }
    parseOptions(Array("--properties", propsFile.getPath)) match {
      case Success(options) => {
        assertOptions(options, properties = List(propsFile))
      }
      case _ =>
        fail("expected options but failed")
    }
  }
  
  "Options with properties option and multiple existing properties file" should "parse" in {
    val propsFileA = createFile("gwen-a.properties")
    val propsFileB = createFile("gwen-b.properties")
    parseOptions(Array("-p", propsFileA.getPath + "," + propsFileB.getPath)) match {
      case Success(options) => {
        assertOptions(options, properties = List(propsFileA, propsFileB))
      }
      case _ =>
        fail("expected options but failed")
    }
    parseOptions(Array("--properties", propsFileA.getPath + "," + propsFileB.getPath)) match {
      case Success(options) => {
        assertOptions(options, properties = List(propsFileA, propsFileB))
      }
      case _ =>
        fail("expected options but failed")
    }
  }
  
  "Options with tags option with no tags" should "not parse" in {
     parseOptions(Array("-t")) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case _ => 
    }
    parseOptions(Array("--tags")) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case _ => 
    }
  }
  
  "Options with tags option and valid single include tag" should "parse" in {
    parseOptions(Array("-t", "@wip")) match {
      case Success(options) => {
        assertOptions(options, tags = List((Tag("@wip"), true)))
      }
      case _ =>
        fail("expected options but failed")
    }
    parseOptions(Array("--tags", "@wip")) match {
      case Success(options) => {
        assertOptions(options, tags = List((Tag("@wip"), true)))
      }
      case _ =>
        fail("expected options but failed")
    }
  }
  
  "Options with tags option and valid single exclude tag" should "parse" in {
    parseOptions(Array("-t", "~@wip")) match {
      case Success(options) => {
        assertOptions(options, tags = List((Tag("@wip"), false)))
      }
      case _ =>
        fail("expected options but failed")
    }
    parseOptions(Array("--tags", "~@wip")) match {
      case Success(options) => {
        assertOptions(options, tags = List((Tag("@wip"), false)))
      }
      case _ =>
        fail("expected options but failed")
    }
  }
  
  "Options with tags option and invalid single tag" should "not parse" in {
    parseOptions(Array("-t", "wip")) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case _ => 
    }
    parseOptions(Array("--tags", "!wip")) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case _ =>
    }
  }
  
  "Options with tags option and mutiple includes" should "parse" in {
    val tags = "@wip,@regression,@transactional,@simple"
    val expected: List[(Tag, Boolean)] = List((Tag("@wip"), true), (Tag("@regression"), true), (Tag("@transactional"), true), (Tag("@simple"), true))
    parseOptions(Array("-t", tags)) match {
      case Success(options) => {
        assertOptions(options, tags = expected)
      }
      case _ =>
        fail("expected options but failed")
    }
    parseOptions(Array("--tags", tags)) match {
      case Success(options) => {
        assertOptions(options, tags = expected)
      }
      case _ =>
        fail("expected options but failed")
    }
  }
  
  "Options with tags option and mutiple exclude tags" should "parse" in {
    val tags = "~@experimental,~@complex"
    val expected: List[(Tag, Boolean)] = List((Tag("@experimental"), false), (Tag("@complex"), false))
    parseOptions(Array("-t", tags)) match {
      case Success(options) => {
        assertOptions(options, tags = expected)
      }
      case _ =>
        fail("expected options but failed")
    }
    parseOptions(Array("--tags", tags)) match {
      case Success(options) => {
        assertOptions(options, tags = expected)
      }
      case _ =>
        fail("expected options but failed")
    }
  }
  
  "Options with tags option and mutiple include and exclude tags" should "parse" in {
    val tags = "@wip,@regression,~@experimental,@transactional,~@complex,@simple"
    val expected: List[(Tag, Boolean)] = List((Tag("@wip"), true), (Tag("@regression"), true), (Tag("@experimental"), false), (Tag("@transactional"), true), (Tag("@complex"), false), (Tag("@simple"), true))
    parseOptions(Array("-t", tags)) match {
      case Success(options) => {
        assertOptions(options, tags = expected)
      }
      case _ =>
        fail("expected options but failed")
    }
    parseOptions(Array("--tags", tags)) match {
      case Success(options) => {
        assertOptions(options, tags = expected)
      }
      case _ =>
        fail("expected options but failed")
    }
  }
  
  "Options with tags option and two tags separated by space" should "not parse" in {
    val tags = "@wip @regression"
    parseOptions(Array("-t", tags)) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case _ =>
    }
    parseOptions(Array("--tags", tags)) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case _ =>
    }
  }
  
  "Options with tags option and one valid tag and one invalid tag" should "not parse" in {
    val tags = "@valid,invalid"
    parseOptions(Array("-t", tags)) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case _ =>
    }
    parseOptions(Array("--tags", tags)) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case _ =>
    }
  }
  
  "Options with meta option with no meta file" should "not parse" in {
     parseOptions(Array("-m")) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case _ => 
    }
    parseOptions(Array("--meta")) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case _ => 
    }
  }
  
  "Options with with non existing meta file" should "not parse" in {
     parseOptions(Array("-m", "nonexisting.meta")) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case _ => 
    }
    parseOptions(Array("--meta", "nonexisting.meta")) match {
      case Success(_) => {
        fail("expected None but got options")
      }
      case _ => 
    }
  }
  
  "Options with one existing meta file" should "parse" in {
    val metaFile = createFile("gwen.meta")
    parseOptions(Array("-m", metaFile.getPath)) match {
      case Success(options) => {
        assertOptions(options, metaFiles = List(metaFile))
      }
      case _ =>
        fail("expected options but failed")
    }
    parseOptions(Array("--meta", metaFile.getPath)) match {
      case Success(options) => {
        assertOptions(options, metaFiles = List(metaFile))
      }
      case _ =>
        fail("expected options but failed")
    }
  }
  
  "Options with multiple existing meta files" should "parse" in {
    val metaFile1 = createFile("gwen1.meta") 
    val metaFile2 = createFile("gwen2.meta")
    val metaFiles = List(metaFile1, metaFile2)
    val metaPaths = metaFiles.map(_.getPath).mkString(",")
    parseOptions(Array("-m", metaPaths)) match {
      case Success(options) => {
        assertOptions(options, metaFiles = metaFiles)
      }
      case _ =>
        fail("expected options but failed")
    }
    parseOptions(Array("--meta", metaPaths)) match {
      case Success(options) => {
        assertOptions(options, metaFiles = metaFiles)
      }
      case _ =>
        fail("expected options but failed")
    }
  }
  
  "Options with multiple existing and one missing meta file" should "not parse" in {
    val metaFile1 = createFile("gwen1.meta")
    val metaFile2 = new File("gwen2.meta")
    val metaFile3 = createFile("gwen3.meta")
    val metaFiles = List(metaFile1, metaFile2, metaFile3)
    val metaPaths = metaFiles.map(_.getPath).mkString(",")
    parseOptions(Array("-m", metaPaths)) match {
      case Success(files) => {
        fail("expected None but got options")
      }
      case _ => 
    }
    parseOptions(Array("--meta", metaPaths)) match {
      case Success(files) => {
        fail("expected None but got options")
      }
      case _ =>
    }
  }
  
  "Options with meta multiple meta files" should "parse" in {
    val metaFile1 = createFile("gwen1.meta") 
    val metaFile2 = createFile("gwen2.meta")
    val metaFiles = List(metaFile1, metaFile2)
    val metaPaths = metaFiles.map(_.getPath).mkString(",")
    parseOptions(Array("-m", metaPaths)) match {
      case Success(options) => {
        assertOptions(options, metaFiles = metaFiles)
      }
      case _ =>
        fail("expected options but failed")
    }
    parseOptions(Array("--meta", metaPaths)) match {
      case Success(options) => {
        assertOptions(options, metaFiles = metaFiles)
      }
      case _ =>
        fail("expected options but failed")
    }
  }
  
  "Options with existing feature file path" should "parse" in {
    
    createDir("dir1")
    val feature1 = createFile("dir1/file1.feature")
    
    parseOptions(Array(feature1.getPath)) match {
      case Success(options) => {
        assertOptions(options, features = List(feature1))
      }
      case _ =>
        fail("expected options but failed")
    }
  }
  
  "Options with existing directory path" should "parse" in {
    
    val dir2 = createDir("dir2")
    
    parseOptions(Array(dir2.getPath)) match {
      case Success(options) => {
        assertOptions(options, features = List(dir2))
      }
      case _ =>
        fail("expected options but failed")
    }
    
  }
  
  "Options with existing paths" should "parse" in {
    
    val dir3 = createDir("dir3")
    val feature3 = createFile("dir3/file3.feature")
    val dir4 = createDir("dir4")
    
    parseOptions(Array(dir3.getPath, feature3.getPath, dir4.getPath)) match {
      case Success(options) => {
        assertOptions(options, features = List(dir3, feature3, dir4))
      }
      case _ =>
        fail("expected options but failed")
    }
  }
  
  "Options with nonexisting paths" should "not parse" in {
    
    parseOptions(Array("nonexistindir", "nonexisting.file")) match {
      case Success(options) => {
        fail("expected None but got options")
      }
      case _ =>
    }
  }
  
  "Options with all valid options" should "parse" in {
    
    val reportDir = new File("target/report")
    val propsFile = createFile("gwen-1.properties")
    val dataFile = createFile("gwen-1.csv")
    val tags = "@wip,@regression,~@experimental,@transactional,~@complex,@simple"
    val metaFile = createFile("gwen.meta")
    val dir5 = createDir("dir5")
    val feature5 = createFile("dir5/file5.feature")
    val dir6 = createDir("dir6")
    
    parseOptions(Array("-b", "--parallel", "-r", reportDir.getPath, "-f", "html,junit", "-p", propsFile.getPath, "-t", tags, "-i", dataFile.getPath, "-m", metaFile.getPath, dir5.getPath, feature5.getPath, dir6.getPath)) match {
      case Success(options) => {
        assertOptions(
          options,
          batch = true,
          parallel = true,
          parallelFeatures = false,
          Some(reportDir),
          List(ReportFormat.html, ReportFormat.junit),
          List(propsFile),
          List((Tag("@wip"), true), (Tag("@regression"), true), (Tag("@experimental"), false), (Tag("@transactional"), true), (Tag("@complex"), false), (Tag("@simple"), true)),
          dryRun = false,
          Some(dataFile),
          List(metaFile),
          List(dir5, feature5, dir6))
      }
      case _ =>
        fail("expected options but failed")
    }
    
    parseOptions(Array("--batch", "--parallel", "--parallel-features", "--report", reportDir.getPath(), "--formats", "html,junit", "--properties", propsFile.getPath(), "--tags", tags, "--input-data", dataFile.getPath(), "--meta", metaFile.getPath(), dir5.getPath(), feature5.getPath(), dir6.getPath)) match {
      case Success(options) => {
        assertOptions(
          options,
          batch = true,
          parallel = true,
          parallelFeatures = true,
          Some(reportDir),
          List(ReportFormat.html, ReportFormat.junit),
          List(propsFile),
          List((Tag("@wip"), true), (Tag("@regression"), true), (Tag("@experimental"), false), (Tag("@transactional"), true), (Tag("@complex"), false), (Tag("@simple"), true)),
          dryRun = false,
          Some(dataFile),
          List(metaFile),
          List(dir5, feature5, dir6))
      }
      case _ =>
        fail("expected options but failed")
    }

    parseOptions(Array("--batch", "--parallel-features", "--report", reportDir.getPath(), "--formats", "html,junit", "--properties", propsFile.getPath(), "--tags", tags, "--input-data", dataFile.getPath(), "--meta", metaFile.getPath(), dir5.getPath(), feature5.getPath(), dir6.getPath)) match {
      case Success(options) => {
        assertOptions(
          options,
          batch = true,
          parallel = false,
          parallelFeatures = true,
          Some(reportDir),
          List(ReportFormat.html, ReportFormat.junit),
          List(propsFile),
          List((Tag("@wip"), true), (Tag("@regression"), true), (Tag("@experimental"), false), (Tag("@transactional"), true), (Tag("@complex"), false), (Tag("@simple"), true)),
          dryRun = false,
          Some(dataFile),
          List(metaFile),
          List(dir5, feature5, dir6))
      }
      case _ =>
        fail("expected options but failed")
    }
    
  }
  
  private def parseOptions(args: Array[String]): Try[GwenOptions] = Try {
    GwenOptions(args)
  }
    
  private def assertOptions(
                             options: GwenOptions,
                             batch: Boolean = false,
                             parallel: Boolean = false,
                             parallelFeatures: Boolean = false,
                             reportDir: Option[File] = None,
                             reportFormats: List[ReportFormat.Value] = Nil,
                             properties: List[File] = Nil,
                             tags: List[(Tag, Boolean)] = Nil,
                             dryRun: Boolean = false,
                             dataFile: Option[File] = None,
                             metaFiles: List[File] = Nil,
                             features: List[File] = Nil): Unit = {
    
    options.batch should be (batch)
    options.parallel should be (parallel)
    options.parallelFeatures should be (parallelFeatures)
    options.reportDir should be (reportDir)
    options.reportFormats should be (reportFormats)
    options.properties should be (properties)
    options.tags should be (tags)
    options.dryRun should be (dryRun)
    options.dataFile should be (dataFile)
    options.metas should be (FileIO.appendFile(metaFiles, Settings.UserMeta))
    options.features should be (features)
    
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