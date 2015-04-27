/*
 * Copyright 2014-2015 Branko Juric, Brady Wood
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

package gwen.eval

import java.io.File
import scala.reflect.io.Path
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import gwen.dsl.Tag
import gwen.dsl.Tag.string2Tag
import scala.util.Success
import scala.util.Try
import scala.util.Failure
import gwen.UserOverrides
import gwen.sample.math.MathInterpreter

class GwenOptionsTest extends FlatSpec with Matchers {
  
  val rootDir = new File("target" + File.separator + "props")
  Path(rootDir).createDirectory()

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
        error.getMessage() should be ("No feature files and/or directories specified")
    }
    parseOptions(Array("--batch")) match {
      case Success(options) => { 
        fail("expected failure but was successful")
      }
      case Failure(error) => 
        error.getMessage() should be ("No feature files and/or directories specified")
    }
  }
  
  "Options with batch option and files " should "parse" in {
    parseOptions(Array("-b", ".")) match {
      case Success(options) => { 
        assertOptions(options, true, paths = List(new File(".")))

      }
      case _ => 
        fail("expected options but failed")
    }
    parseOptions(Array("--batch", ".")) match {
      case Success(options) => { 
        assertOptions(options, true, paths = List(new File(".")))
      }
      case _ => 
        fail("expected options but failed")
    }
  }
  
  "Options with parallel option with implied batch mode" should "parse" in {
    parseOptions(Array("-|", ".")) match {
      case Success(options) => { 
        assertOptions(options, batch = true, parallel = true, paths = List(new File(".")))
      }
      case _ => fail("expected options but failed")
    }
    parseOptions(Array("--parallel", ".")) match {
      case Success(options) => { 
        assertOptions(options, batch = true, parallel = true, paths = List(new File(".")))
      }
      case _ => fail("expected options but failed")
    }
  }
  
  "Options with parallel option with explicit batch mode" should "parse" in {
    parseOptions(Array("-|b", ".")) match {
      case Success(options) => { 
        assertOptions(options, batch = true, parallel = true, paths = List(new File(".")))
      }
      case _ => fail("expected options but failed")
    }
    parseOptions(Array("--parallel", "--batch", ".")) match {
      case Success(options) => { 
        assertOptions(options, batch = true, parallel = true, paths = List(new File(".")))
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
        assertOptions(options, reportDir = Some(new File("target/report")))
      }
      case _ =>
        fail("expected options but failed")
    }
    parseOptions(Array("--report", "target/report")) match {
      case Success(options) => {
        assertOptions(options, reportDir = Some(new File("target/report")))
      }
      case _ =>
        fail("expected options but failed")
    }
  }
  
  "Options with properties option with no properties file" should "not parse" in {
     parseOptions(Array("-p")) match {
      case Success(options) => {
        fail("expected None but got options")
      }
      case _ => 
    }
    parseOptions(Array("--properties")) match {
      case Success(options) => {
        fail("expected None but got options")
      }
      case _ => 
    }
  }
  
  "Options with properties option with non existing properties file" should "not parse" in {
     parseOptions(Array("-p", "nonexisting.properties")) match {
      case Success(options) => {
        fail("expected None but got options")
      }
      case _ => 
    }
    parseOptions(Array("--properties", "nonexisting.properties")) match {
      case Success(options) => {
        fail("expected None but got options")
      }
      case _ => 
    }
  }
  
  "Options with properties option and existing properties file" should "parse" in {
    val propsFile = createFile("gwen.properties")
    parseOptions(Array("-p", propsFile.getPath())) match {
      case Success(options) => {
        assertOptions(options, properties = List(propsFile))
      }
      case _ =>
        fail("expected options but failed")
    }
    parseOptions(Array("--properties", propsFile.getPath())) match {
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
    parseOptions(Array("-p", propsFileA.getPath() + "," + propsFileB.getPath())) match {
      case Success(options) => {
        assertOptions(options, properties = List(propsFileA, propsFileB))
      }
      case _ =>
        fail("expected options but failed")
    }
    parseOptions(Array("--properties", propsFileA.getPath() + "," + propsFileB.getPath())) match {
      case Success(options) => {
        assertOptions(options, properties = List(propsFileA, propsFileB))
      }
      case _ =>
        fail("expected options but failed")
    }
  }
  
  "Options with tags option with no tags" should "not parse" in {
     parseOptions(Array("-t")) match {
      case Success(options) => {
        fail("expected None but got options")
      }
      case _ => 
    }
    parseOptions(Array("--tags")) match {
      case Success(options) => {
        fail("expected None but got options")
      }
      case _ => 
    }
  }
  
  "Options with tags option and valid single include tag" should "parse" in {
    parseOptions(Array("-t", "@wip")) match {
      case Success(options) => {
        assertOptions(options, tags = List(("@wip", true)))
      }
      case _ =>
        fail("expected options but failed")
    }
    parseOptions(Array("--tags", "@wip")) match {
      case Success(options) => {
        assertOptions(options, tags = List(("@wip", true)))
      }
      case _ =>
        fail("expected options but failed")
    }
  }
  
  "Options with tags option and valid single exclude tag" should "parse" in {
    parseOptions(Array("-t", "~@wip")) match {
      case Success(options) => {
        assertOptions(options, tags = List(("@wip", false)))
      }
      case _ =>
        fail("expected options but failed")
    }
    parseOptions(Array("--tags", "~@wip")) match {
      case Success(options) => {
        assertOptions(options, tags = List(("@wip", false)))
      }
      case _ =>
        fail("expected options but failed")
    }
  }
  
  "Options with tags option and invalid single tag" should "not parse" in {
    parseOptions(Array("-t", "wip")) match {
      case Success(options) => {
        fail("expected None but got options")
      }
      case _ => 
    }
    parseOptions(Array("--tags", "!wip")) match {
      case Success(options) => {
        fail("expected None but got options")
      }
      case _ =>
    }
  }
  
  "Options with tags option and mutiple includes" should "parse" in {
    val tags = "@wip,@regression,@transactional,@simple"
    val expected: List[(Tag, Boolean)] = List(("@wip", true), ("@regression", true), ("@transactional", true), ("@simple", true))
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
    val expected: List[(Tag, Boolean)] = List(("@experimental", false), ("@complex", false))
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
    val expected: List[(Tag, Boolean)] = List(("@wip", true), ("@regression", true), ("@experimental", false), ("@transactional", true), ("@complex", false), ("@simple", true))
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
      case Success(options) => {
        fail("expected None but got options")
      }
      case _ =>
    }
    parseOptions(Array("--tags", tags)) match {
      case Success(options) => {
        fail("expected None but got options")
      }
      case _ =>
    }
  }
  
  "Options with tags option and one valid tag and one invalid tag" should "not parse" in {
    val tags = "@valid,invalid"
    parseOptions(Array("-t", tags)) match {
      case Success(options) => {
        fail("expected None but got options")
      }
      case _ =>
    }
    parseOptions(Array("--tags", tags)) match {
      case Success(options) => {
        fail("expected None but got options")
      }
      case _ =>
    }
  }
  
  "Options with meta option with no meta file" should "not parse" in {
     parseOptions(Array("-m")) match {
      case Success(options) => {
        fail("expected None but got options")
      }
      case _ => 
    }
    parseOptions(Array("--meta")) match {
      case Success(options) => {
        fail("expected None but got options")
      }
      case _ => 
    }
  }
  
  "Options with with non existing meta file" should "not parse" in {
     parseOptions(Array("-m", "nonexisting.meta")) match {
      case Success(options) => {
        fail("expected None but got options")
      }
      case _ => 
    }
    parseOptions(Array("--meta", "nonexisting.meta")) match {
      case Success(options) => {
        fail("expected None but got options")
      }
      case _ => 
    }
  }
  
  "Options with one existing meta file" should "parse" in {
    val metaFile = createFile("gwen.meta")
    parseOptions(Array("-m", metaFile.getPath())) match {
      case Success(options) => {
        assertOptions(options, metaFiles = List(metaFile))
      }
      case _ =>
        fail("expected options but failed")
    }
    parseOptions(Array("--meta", metaFile.getPath())) match {
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
    
    val dir1 = createDir("dir1");
    val feature1 = createFile("dir1/file1.feature");
    
    parseOptions(Array(feature1.getPath())) match {
      case Success(options) => {
        assertOptions(options, paths = List(feature1))
      }
      case _ =>
        fail("expected options but failed")
    }
  }
  
  "Options with existing directory path" should "parse" in {
    
    val dir2 = createDir("dir2");
    
    parseOptions(Array(dir2.getPath())) match {
      case Success(options) => {
        assertOptions(options, paths = List(dir2))
      }
      case _ =>
        fail("expected options but failed")
    }
    
  }
  
  "Options with existing paths" should "parse" in {
    
    val dir3 = createDir("dir3");
    val feature3 = createFile("dir3/file3.feature");
    val dir4 = createDir("dir4");
    
    parseOptions(Array(dir3.getPath(), feature3.getPath(), dir4.getPath())) match {
      case Success(options) => {
        assertOptions(options, paths = List(dir3, feature3, dir4))
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
    val tags = "@wip,@regression,~@experimental,@transactional,~@complex,@simple"
    val metaFile = createFile("gwen.meta")
    val dir5 = createDir("dir5");
    val feature5 = createFile("dir5/file5.feature");
    val dir6 = createDir("dir6");
    
    parseOptions(Array("-b|", "-r", reportDir.getPath(), "-p", propsFile.getPath(), "-t", tags, "-m", metaFile.getPath(), dir5.getPath(), feature5.getPath(), dir6.getPath)) match {
      case Success(options) => {
        assertOptions(
          options,
          true,
          true,
          Some(reportDir),
          List(propsFile),
          List(("@wip", true), ("@regression", true), ("@experimental", false), ("@transactional", true), ("@complex", false), ("@simple", true)),
          List(metaFile),
          List(dir5, feature5, dir6))
      }
      case _ =>
        fail("expected options but failed")
    }
    
    parseOptions(Array("--batch", "--parallel", "--report", reportDir.getPath(), "--properties", propsFile.getPath(), "--tags", tags, "--meta", metaFile.getPath(), dir5.getPath(), feature5.getPath(), dir6.getPath)) match {
      case Success(options) => {
        assertOptions(
          options,
          true,
          true,
          Some(reportDir),
          List(propsFile),
          List(("@wip", true), ("@regression", true), ("@experimental", false), ("@transactional", true), ("@complex", false), ("@simple", true)),
          List(metaFile),
          List(dir5, feature5, dir6))
      }
      case _ =>
        fail("expected options but failed")
    }
    
  }
  
  private def parseOptions(args: Array[String]): Try[GwenOptions] = Try {
    GwenOptions(MathInterpreter.getClass, args)
  }
    
  private def assertOptions(
    options: GwenOptions, 
    batch: Boolean = false,
    parallel: Boolean = false,
    reportDir: Option[File] = None,
    properties: List[File] = Nil,
    tags: List[(Tag, Boolean)] = Nil,
    metaFiles: List[File] = Nil, 
    paths: List[File] = Nil) {
    
    options.batch should be (batch)
    options.parallel should be (parallel)
    options.reportDir should be (reportDir)
    options.properties should be (UserOverrides.addUserProperties(properties))
    options.tags should be (tags)
    options.metaFiles should be (UserOverrides.addUserMeta(metaFiles))
    options.paths should be (paths)
    
  }
  
  private def createFile(filepath: String): File = {
    val file = new File(rootDir + File.separator + filepath.replace('/', File.separatorChar))
    val path = Path(file)
    if (path.exists) {
      path.delete()
    }
    path.createFile(true)
    file
  }
  
  private def createDir(dirname: String): File = {
    val dir = new File(rootDir, dirname)
    val path = Path(dir)
    path.deleteRecursively()
    path.createDirectory()
    dir
  }
  
  
  
}