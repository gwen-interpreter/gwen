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
import java.util.NoSuchElementException

import scala.reflect.io.Path

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class FeatureStreamTest extends FlatSpec with Matchers {
  
  val rootDir = new File("target" + File.separator + "features")
  Path(rootDir).createDirectory()
  
  "Directory with no feature files" should "result in empty suite" in {
    FeatureStream.read(createDir("dir1")).size should be (0)
  }
  
  "Directory with non feature files" should "return empty suite" in {
    createDir("dir2");
    FeatureStream.read(createFile("dir2/file.meta")).size should be (0)
    FeatureStream.read(createFile("dir2/file.text")).size should be (0)
    FeatureStream.read(createFile("dir2/feature")).size should be (0)
  }
  
  it should "not find feature files above the specified directory" in {
     createDir("dir3")
    createFile("dir3/file.feature")
    createFile("dir3/file.meta")
    FeatureStream.read(createDir("dir3/dir4")).size should be (0)
  }
  
  "1 input feature file with no meta" should "return the 1 feature file only" in {
    createDir("dir4")
    val featureFile = createFile("dir4/file.feature")
    val suite = FeatureStream.read(featureFile)
    suite match {
      case unit #:: Stream() => 
        assertFeatureFile(featureFile, unit.featureFile)
        assertMetaFiles(Nil, unit.metaFiles)
      case _ =>
        fail(s"1 feature unit expected but ${suite.size} found")
    }
  }
  
  "1 input feature file with one meta in same dir" should "return the 1 feature and 1 meta" in {
    val dir         =  createDir("dir5")
    val featureFile = createFile("dir5/file.feature")
    val metaFile    = createFile("dir5/file.meta")
    val suite = FeatureStream.read(dir)
    suite match {
      case unit #:: Stream() => 
        assertFeatureFile(featureFile, unit.featureFile)
        assertMetaFiles(List(metaFile), unit.metaFiles)
      case _ =>
        fail(s"1 feature unit expected but ${suite.size} found")
    }
  }
  
  "1 input feature file with 1 meta in same dir and 1 meta in parent" should "return the 1 feature and 2 meta" in {
    val dir6        =  createDir("dir6")
    val dir7        =  createDir("dir6/dir7")
    val metaFile1   = createFile("dir6/file1.meta")
    val metaFile2   = createFile("dir6/dir7/file2.meta")
    val featureFile = createFile("dir6/dir7/file.feature")
    val suite = FeatureStream.read(dir6)
    suite match {
      case unit #:: Stream() => 
        assertFeatureFile(featureFile, unit.featureFile)
        assertMetaFiles(List(metaFile1, metaFile2), unit.metaFiles)
      case _ =>
        fail(s"1 feature unit expected but ${suite.size} found")
    }
  }
  
  "1 input feature file with no meta in same dir but 1 meta in parent " should "return the 1 feature and 1 meta" in {
    val dir8        =  createDir("dir8")
    val dir9        =  createDir("dir8/dir9")
    val metaFile    = createFile("dir8/file.meta")
    val featureFile = createFile("dir8/dir9/file.feature")
    val suite = FeatureStream.read(dir8)
    suite match {
      case unit #:: Stream() => 
        assertFeatureFile(featureFile, unit.featureFile)
        assertMetaFiles(List(metaFile), unit.metaFiles)
      case _ =>
        fail(s"1 feature unit expected but ${suite.size} found")
    }
  }
  
  "feature file with parent directory containing meta" should "return 1 feature and accumulated meta" in {
    val dir10       =  createDir("dir10")
    val dir11       =  createDir("dir10/dir11")
    val dir12       =  createDir("dir10/dir11/dir12")
    val metaFile1   = createFile("dir10/file1.meta")
    val metaFile2   = createFile("dir10/dir11/dir12/file2.meta")
    val featureFile = createFile("dir10/dir11/dir12/file.feature")
    val suite = FeatureStream.read(featureFile)
    suite match {
      case unit #:: Stream() => 
        assertFeatureFile(featureFile, unit.featureFile)
        assertMetaFiles(List(metaFile1, metaFile2), unit.metaFiles)
      case _ =>
        fail(s"1 feature unit expected but ${suite.size} found")
    }
  }
  
  "multi suite stream" should "be read in correctly" in {
    
    val dirA           =  createDir("dirA")
    val metaFileA      = createFile("dirA/fileA.meta")
    val featureFileA   = createFile("dirA/fileA.feature")
    val dirAB          =  createDir("dirA/dirB")
    val metaFileAB     = createFile("dirA/dirB/fileAB.meta")
    val featureFileAB  = createFile("dirA/dirB/fileAB.feature")
    val dirAB1         =  createDir("dirA/dirB/dir1")
    val featureFileAB1 = createFile("dirA/dirB/dir1/fileAB1.feature")
    val dirAB2         =  createDir("dirA/dirB/dir2")
    val featureFileAB2 = createFile("dirA/dirB/dir2/fileAB2.feature")
    val metaFileAB2    = createFile("dirA/dirB/dir2/fileAB2.meta")
    val dirD           =  createDir("dirD")
    val featureFile1D  = createFile("dirD/file1D.feature")
    val featureFile2D  = createFile("dirD/file2D.feature")
    val dirDE          =  createDir("dirD/dirE")
    val metaFileDE     = createFile("dirD/dirE/fileDE.meta")
    val featureFileDE  = createFile("dirD/dirE/fileDE.feature")
    val dirF           =  createDir("dirF")
    val metaFileF      = createFile("dirF/fileF.meta")
    
    val suiteStream = FeatureStream.readAll(List(dirA, dirAB, dirAB1, dirAB2, featureFileAB2, dirD, featureFile2D, dirDE, dirF))
    val suites = suiteStream.iterator

    // dirA suite
    var suite = suites.next
    var units = suite.toList.sortBy(_.featureFile).iterator
    var unit = units.next
    assertFeatureFile(featureFileAB1, unit.featureFile)
    assertMetaFiles(List(metaFileA, metaFileAB), unit.metaFiles)
    unit = units.next
    assertFeatureFile(featureFileAB2, unit.featureFile)
    assertMetaFiles(List(metaFileA, metaFileAB, metaFileAB2), unit.metaFiles)
    unit = units.next
    assertFeatureFile(featureFileAB, unit.featureFile)
    assertMetaFiles(List(metaFileA, metaFileAB), unit.metaFiles)
    unit = units.next
    assertFeatureFile(featureFileA, unit.featureFile)
    assertMetaFiles(List(metaFileA), unit.metaFiles)
    assertEndOfStream(units.next)
    
    // dirAB suite
    suite = suites.next
    units = suite.toList.sortBy(_.featureFile).iterator
    unit = units.next
    assertFeatureFile(featureFileAB1, unit.featureFile)
    assertMetaFiles(List(metaFileA, metaFileAB), unit.metaFiles)
    unit = units.next
    assertFeatureFile(featureFileAB2, unit.featureFile)
    assertMetaFiles(List(metaFileA, metaFileAB, metaFileAB2), unit.metaFiles)
    unit = units.next
    assertFeatureFile(featureFileAB, unit.featureFile)
    assertMetaFiles(List(metaFileA, metaFileAB), unit.metaFiles)
    assertEndOfStream(units.next)
    
    // dir AB1 suite
    suite = suites.next
    units = suite.toList.sortBy(_.featureFile).iterator
    unit = units.next
    assertFeatureFile(featureFileAB1, unit.featureFile)
    assertMetaFiles(List(metaFileA, metaFileAB), unit.metaFiles)
    assertEndOfStream(units.next)
    
    // dir AB2 suite
    suite = suites.next
    units = suite.toList.sortBy(_.featureFile).iterator
    unit = units.next
    assertFeatureFile(featureFileAB2, unit.featureFile)
    assertMetaFiles(List(metaFileA, metaFileAB, metaFileAB2), unit.metaFiles)
    assertEndOfStream(units.next)
    
    // featureFile AB2 suite
    suite = suites.next
    units = suite.toList.sortBy(_.featureFile).iterator
    unit = units.next
    assertFeatureFile(featureFileAB2, unit.featureFile)
    assertMetaFiles(List(metaFileA, metaFileAB, metaFileAB2), unit.metaFiles)
    assertEndOfStream(units.next)
    
    // dir D suite
    suite = suites.next
    units = suite.toList.sortBy(_.featureFile).iterator
    unit = units.next
    assertFeatureFile(featureFileDE, unit.featureFile)
    assertMetaFiles(List(metaFileDE), unit.metaFiles)
    unit = units.next
    assertFeatureFile(featureFile1D, unit.featureFile)
    assertMetaFiles(Nil, unit.metaFiles)
    unit = units.next
    assertFeatureFile(featureFile2D, unit.featureFile)
    assertMetaFiles(Nil, unit.metaFiles)
    assertEndOfStream(units.next)
    
    // featureFile 2D suite
    suite = suites.next
    units = suite.toList.sortBy(_.featureFile).iterator
    unit = units.next
    assertFeatureFile(featureFile2D, unit.featureFile)
    assertMetaFiles(Nil, unit.metaFiles)
    assertEndOfStream(units.next)
    
    // dir DE suite
    suite = suites.next
    units = suite.toList.sortBy(_.featureFile).iterator
    unit = units.next
    assertFeatureFile(featureFileDE, unit.featureFile)
    assertMetaFiles(List(metaFileDE), unit.metaFiles)
    assertEndOfStream(units.next)
    
    // dir F suite
    suite = suites.next
    units = suite.toList.sortBy(_.featureFile).iterator
    assertEndOfStream(units.next)
    
  }
  
  private def createFile(filepath: String): File = {
    val file = new File(rootDir + File.separator + filepath.replace('/', File.separatorChar))
    val path = Path(file)
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
  
  private def assertFeatureFile(expected: File, actual: File) {
    actual.getPath.startsWith("target") should be (true)
    actual.getPath should be (expected.getPath)
  }
  
  private def assertMetaFiles(expecteds: List[File], actuals: List[File]) {
    expecteds zip actuals foreach { case (expected, actual) =>
      val path = actual.getPath() 
      path.startsWith("target") should be (true)
      path should be (expected.getPath)
    }
  }
  
  private def assertEndOfStream(next: () => Unit) {
    intercept[NoSuchElementException] {
    	next()
    }
  }
  
}