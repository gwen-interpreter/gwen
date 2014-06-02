/*
 * Copyright 2014 Branko Juric, Brady Wood
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

class GwenOptionsTest extends FlatSpec with Matchers {
  
  val rootDir = new File("target" + File.separator + "props")
  Path(rootDir).createDirectory()

  "Options with no command line args" should "parse" in {
    parseOptions(Array[String]()) match {
      case Some(options) => { 
        assertOptions(options)
      }
      case None => fail("expected options but got None")
    }
  }
  
  "Options with batch option" should "parse" in {
    parseOptions(Array("-b")) match {
      case Some(options) => { 
        assertOptions(options, true)
      }
      case None => fail("expected options but got None")
    }
    parseOptions(Array("--batch")) match {
      case Some(options) => { 
        assertOptions(options, true)
      }
      case None => fail("expected options but got None")
    }
  }
  
  "Options with parallel option" should "parse" in {
    parseOptions(Array("-|")) match {
      case Some(options) => { 
        assertOptions(options, parallel = true)
      }
      case None => fail("expected options but got None")
    }
    parseOptions(Array("--parallel")) match {
      case Some(options) => { 
        assertOptions(options, parallel = true)
      }
      case None => fail("expected options but got None")
    }
  }
  
  "Options with report option but no target report directory" should "not parse" in {
    parseOptions(Array("-r")) match {
      case Some(options) => {
        fail("expected None but got options")
      }
      case None => 
    }
    parseOptions(Array("--report")) match {
      case Some(options) => {
        fail("expected None but got options")
      }
      case None => 
    }
  }
  
  "Options with report option and report directory" should "parse" in {
    parseOptions(Array("-r", "target/report")) match {
      case Some(options) => {
        assertOptions(options, reportDir = Some(new File("target/report")))
      }
      case None =>
        fail("expected options but got None")
    }
    parseOptions(Array("--report", "target/report")) match {
      case Some(options) => {
        assertOptions(options, reportDir = Some(new File("target/report")))
      }
      case None =>
        fail("expected options but got None")
    }
  }
  
  "Options with properties option with no properties file" should "not parse" in {
     parseOptions(Array("-p")) match {
      case Some(options) => {
        fail("expected None but got options")
      }
      case None => 
    }
    parseOptions(Array("--properties")) match {
      case Some(options) => {
        fail("expected None but got options")
      }
      case None => 
    }
  }
  
  "Options with properties option with non existing properties file" should "not parse" in {
     parseOptions(Array("-p", "nonexisting.properties")) match {
      case Some(options) => {
        fail("expected None but got options")
      }
      case None => 
    }
    parseOptions(Array("--properties", "nonexisting.properties")) match {
      case Some(options) => {
        fail("expected None but got options")
      }
      case None => 
    }
  }
  
  "Options with properties option and existing properties file" should "parse" in {
    val propsFile = createFile("gwen.properties")
    parseOptions(Array("-p", propsFile.getPath())) match {
      case Some(options) => {
        assertOptions(options, propertiesFile = Some(propsFile))
      }
      case None =>
        fail("expected options but got None")
    }
    parseOptions(Array("--properties", propsFile.getPath())) match {
      case Some(options) => {
        assertOptions(options, propertiesFile = Some(propsFile))
      }
      case None =>
        fail("expected options but got None")
    }
  }
  
  "Options with tags option with no tags" should "not parse" in {
     parseOptions(Array("-t")) match {
      case Some(options) => {
        fail("expected None but got options")
      }
      case None => 
    }
    parseOptions(Array("--tags")) match {
      case Some(options) => {
        fail("expected None but got options")
      }
      case None => 
    }
  }
  
  "Options with tags option and valid single include tag" should "parse" in {
    parseOptions(Array("-t", "@wip")) match {
      case Some(options) => {
        assertOptions(options, tags = List(("@wip", true)))
      }
      case None =>
        fail("expected options but got None")
    }
    parseOptions(Array("--tags", "@wip")) match {
      case Some(options) => {
        assertOptions(options, tags = List(("@wip", true)))
      }
      case None =>
        fail("expected options but got None")
    }
  }
  
  "Options with tags option and valid single exclude tag" should "parse" in {
    parseOptions(Array("-t", "~@wip")) match {
      case Some(options) => {
        assertOptions(options, tags = List(("@wip", false)))
      }
      case None =>
        fail("expected options but got None")
    }
    parseOptions(Array("--tags", "~@wip")) match {
      case Some(options) => {
        assertOptions(options, tags = List(("@wip", false)))
      }
      case None =>
        fail("expected options but got None")
    }
  }
  
  "Options with tags option and invalid single tag" should "not parse" in {
    parseOptions(Array("-t", "wip")) match {
      case Some(options) => {
        fail("expected None but got options")
      }
      case None => 
    }
    parseOptions(Array("--tags", "~wip")) match {
      case Some(options) => {
        fail("expected None but got options")
      }
      case None =>
    }
  }
  
  "Options with tags option and mutiple includes" should "parse" in {
    val tags = "@wip,@regression,@transactional,@simple"
    val expected: List[(Tag, Boolean)] = List(("@wip", true), ("@regression", true), ("@transactional", true), ("@simple", true))
    parseOptions(Array("-t", tags)) match {
      case Some(options) => {
        assertOptions(options, tags = expected)
      }
      case None =>
        fail("expected options but got None")
    }
    parseOptions(Array("--tags", tags)) match {
      case Some(options) => {
        assertOptions(options, tags = expected)
      }
      case None =>
        fail("expected options but got None")
    }
  }
  
  "Options with tags option and mutiple exclude tags" should "parse" in {
    val tags = "~@experimental,~@complex"
    val expected: List[(Tag, Boolean)] = List(("@experimental", false), ("@complex", false))
    parseOptions(Array("-t", tags)) match {
      case Some(options) => {
        assertOptions(options, tags = expected)
      }
      case None =>
        fail("expected options but got None")
    }
    parseOptions(Array("--tags", tags)) match {
      case Some(options) => {
        assertOptions(options, tags = expected)
      }
      case None =>
        fail("expected options but got None")
    }
  }
  
  "Options with tags option and mutiple include and exclude tags" should "parse" in {
    val tags = "@wip,@regression,~@experimental,@transactional,~@complex,@simple"
    val expected: List[(Tag, Boolean)] = List(("@wip", true), ("@regression", true), ("@experimental", false), ("@transactional", true), ("@complex", false), ("@simple", true))
    parseOptions(Array("-t", tags)) match {
      case Some(options) => {
        assertOptions(options, tags = expected)
      }
      case None =>
        fail("expected options but got None")
    }
    parseOptions(Array("--tags", tags)) match {
      case Some(options) => {
        assertOptions(options, tags = expected)
      }
      case None =>
        fail("expected options but got None")
    }
  }
  
  "Options with tags option and two tags separated by space" should "not parse" in {
    val tags = "@wip @regression"
    parseOptions(Array("-t", tags)) match {
      case Some(options) => {
        fail("expected None but got options")
      }
      case None =>
    }
    parseOptions(Array("--tags", tags)) match {
      case Some(options) => {
        fail("expected None but got options")
      }
      case None =>
    }
  }
  
  "Options with tags option and one valid tag and one invalid tag" should "not parse" in {
    val tags = "@valid,invalid"
    parseOptions(Array("-t", tags)) match {
      case Some(options) => {
        fail("expected None but got options")
      }
      case None =>
    }
    parseOptions(Array("--tags", tags)) match {
      case Some(options) => {
        fail("expected None but got options")
      }
      case None =>
    }
  }
  
  "Options with meta option with no meta file" should "not parse" in {
     parseOptions(Array("-m")) match {
      case Some(options) => {
        fail("expected None but got options")
      }
      case None => 
    }
    parseOptions(Array("--meta")) match {
      case Some(options) => {
        fail("expected None but got options")
      }
      case None => 
    }
  }
  
  "Options with meta option with non existing meta file" should "not parse" in {
     parseOptions(Array("-m", "nonexisting.meta")) match {
      case Some(options) => {
        fail("expected None but got options")
      }
      case None => 
    }
    parseOptions(Array("--meta", "nonexisting.meta")) match {
      case Some(options) => {
        fail("expected None but got options")
      }
      case None => 
    }
  }
  
  "Options with meta option and existing meta file" should "parse" in {
    val metaFile = createFile("gwen.meta")
    parseOptions(Array("-m", metaFile.getPath())) match {
      case Some(options) => {
        assertOptions(options, metaFile = Some(metaFile))
      }
      case None =>
        fail("expected options but got None")
    }
    parseOptions(Array("--meta", metaFile.getPath())) match {
      case Some(options) => {
        assertOptions(options, metaFile = Some(metaFile))
      }
      case None =>
        fail("expected options but got None")
    }
  }
  
  "Options with existing feature file path" should "parse" in {
    
    val dir1 = createDir("dir1");
    val feature1 = createFile("dir1/file1.feature");
    
    parseOptions(Array(feature1.getPath())) match {
      case Some(options) => {
        assertOptions(options, paths = List(feature1))
      }
      case None =>
        fail("expected options but got None")
    }
  }
  
  "Options with existing directory path" should "parse" in {
    
    val dir2 = createDir("dir2");
    
    parseOptions(Array(dir2.getPath())) match {
      case Some(options) => {
        assertOptions(options, paths = List(dir2))
      }
      case None =>
        fail("expected options but got None")
    }
    
  }
  
  "Options with existing paths" should "parse" in {
    
    val dir3 = createDir("dir3");
    val feature3 = createFile("dir3/file3.feature");
    val dir4 = createDir("dir4");
    
    parseOptions(Array(dir3.getPath(), feature3.getPath(), dir4.getPath())) match {
      case Some(options) => {
        assertOptions(options, paths = List(dir3, feature3, dir4))
      }
      case None =>
        fail("expected options but got None")
    }
  }
  
  "Options with nonexisting paths" should "not parse" in {
    
    parseOptions(Array("nonexistindir", "nonexisting.file")) match {
      case Some(options) => {
        fail("expected None but got options")
      }
      case None =>
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
      case Some(options) => {
        assertOptions(
          options,
          true,
          true,
          Some(reportDir),
          Some(propsFile),
          List(("@wip", true), ("@regression", true), ("@experimental", false), ("@transactional", true), ("@complex", false), ("@simple", true)),
          Some(metaFile),
          List(dir5, feature5, dir6))
      }
      case None =>
        fail("expected options but got None")
    }
    
    parseOptions(Array("--batch", "--parallel", "--report", reportDir.getPath(), "--properties", propsFile.getPath(), "--tags", tags, "--meta", metaFile.getPath(), dir5.getPath(), feature5.getPath(), dir6.getPath)) match {
      case Some(options) => {
        assertOptions(
          options,
          true,
          true,
          Some(reportDir),
          Some(propsFile),
          List(("@wip", true), ("@regression", true), ("@experimental", false), ("@transactional", true), ("@complex", false), ("@simple", true)),
          Some(metaFile),
          List(dir5, feature5, dir6))
      }
      case None =>
        fail("expected options but got None")
    }
    
  }
  
  private def parseOptions(args: Array[String]): Option[GwenOptions] =
    GwenOptions.parse("GwenInterpreter", args)
    
  private def assertOptions(
    options: GwenOptions, 
    batch: Boolean = false,
    parallel: Boolean = false,
    reportDir: Option[File] = None,
    propertiesFile: Option[File] = None,
    tags: List[(Tag, Boolean)] = Nil,
    metaFile: Option[File] = None, 
    paths: List[File] = Nil) {
    
    options.batch should be (batch)
    options.parallel should be (parallel)
    options.reportDir should be (reportDir)
    options.propertiesFile should be (propertiesFile)
    options.tags should be (tags)
    options.metaFile should be (metaFile)
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