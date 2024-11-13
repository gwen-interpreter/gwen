/*
 * Copyright 2014-2024 Branko Juric, Brady Wood
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

package gwen.core.node

import gwen.core._
import gwen.core.Errors.AmbiguousCaseException
import gwen.core.node.gherkin.TagFilter

import org.scalatest.matchers.should.Matchers

import scala.util.chaining._

import java.io.File
import java.util.NoSuchElementException

class FeatureStreamTest extends BaseTest with Matchers {

  val featureStream = new FeatureStream(Nil, new TagFilter(Nil))

  val rootDir: File = new File("target" + File.separator + "features") tap { _.mkdirs() }

  "Directory with no feature files" should "result in empty suite" in {
    featureStream.read(createDir("dir1"), None).size should be (0)
  }

  "Directory with non feature files" should "return empty suite" in {
    createDir("dir2")
    featureStream.read(createFile("dir2/file.meta"), None).size should be (0)
    featureStream.read(createFile("dir2/file.text"), None).size should be (0)
    featureStream.read(createFile("dir2/feature"), None).size should be (0)
  }

  it should "not find feature files above the specified directory" in {
    createDir("dir3")
    createFile("dir3/file.feature")
    createFile("dir3/file.meta")
    featureStream.read(createDir("dir3/dir4"), None).size should be (0)
  }

  "1 input feature file with no meta" should "return the 1 feature file only" in {
    createDir("dir4")
    val featureFile = createFile("dir4/file.feature")
    val suite = featureStream.read(featureFile, None)
    suite.toList match {
      case unit :: Nil =>
        assertFile(featureFile, unit.featureFile)
        assertMetaFiles(Nil, unit.metaFiles)
        unit.dataRecord should be (None)
      case _ =>
        fail(s"1 feature unit expected but ${suite.size} found")
    }
  }

  "1 input feature file with one meta in same dir" should "return the 1 feature and 1 meta" in {
    val dir         = createDir("dir5")
    val featureFile = createFile("dir5/file.feature")
    val metaFile    = createFile("dir5/file.meta")
    val suite = featureStream.read(dir, None)
    suite.toList match {
      case unit :: Nil =>
        assertFile(featureFile, unit.featureFile)
        assertMetaFiles(List(metaFile), unit.metaFiles)
        unit.dataRecord should be (None)
      case _ =>
        fail(s"1 feature unit expected but ${suite.size} found")
    }
  }

  "1 input feature file with 1 meta in same dir and 1 meta in parent" should "return the feature file only" in {
    val dir6 = createDir("dir6")
    createDir("dir6/dir7")
    val metaFile1   = createFile("dir6/file1.meta")
    val metaFile2   = createFile("dir6/dir7/file2.meta")
    val featureFile = createFile("dir6/dir7/file.feature")
    val suite = featureStream.read(dir6, None)
    suite.toList match {
      case unit :: Nil =>
        assertFile(featureFile, unit.featureFile)
        assertMetaFiles(Nil, unit.metaFiles)
        unit.dataRecord should be (None)
      case _ =>
        fail(s"1 feature unit expected but ${suite.size} found")
    }
  }

  "1 input feature file with no meta in same dir but 1 meta in parent " should "return the 1 feature and 1 meta" in {
    val dir8        =  createDir("dir8")
    createDir("dir8/dir9")
    val metaFile    = createFile("dir8/file.meta")
    val featureFile = createFile("dir8/dir9/file.feature")
    val suite = featureStream.read(dir8, None)
    suite.toList match {
      case unit :: Nil =>
        assertFile(featureFile, unit.featureFile)
        assertMetaFiles(List(metaFile), unit.metaFiles)
        unit.dataRecord should be (None)
      case _ =>
        fail(s"1 feature unit expected but ${suite.size} found")
    }
  }

  "feature file with parent directory containing meta" should "return 1 feature and no meta" in {
    createDir("dir10")
    createDir("dir10/dir11")
    createDir("dir10/dir11/dir12")
    val metaFile1   = createFile("dir10/file1.meta")
    val metaFile2   = createFile("dir10/dir11/dir12/file2.meta")
    val metaFile3   = createFile("dir10/dir11/dir12/file3.meta")
    val featureFile = createFile("dir10/dir11/dir12/file.feature")
    val suite = featureStream.read(featureFile, None)
    suite.toList match {
      case unit :: Nil =>
        assertFile(featureFile, unit.featureFile)
        assertMetaFiles(Nil, unit.metaFiles.sortBy(_.getName()))
        unit.dataRecord should be (None)
      case _ =>
        fail(s"1 feature unit expected but ${suite.size} found")
    }
  }

  "multi suite stream" should "be read in correctly" in {

    val dirA           =  createDir("dirA")
    val metaFileA      = createFile("dirA/file.meta")
    val featureFileA   = createFile("dirA/fileA.feature")
    val dirAB          =  createDir("dirA/dirB")
    val metaFileB     = createFile("dirA/dirB/file.meta")
    val featureFileAB  = createFile("dirA/dirB/fileAB.feature")
    val dirAB1         =  createDir("dirA/dirB/dir1")
    val featureFileAB1 = createFile("dirA/dirB/dir1/fileAB1.feature")
    val dirAB2         =  createDir("dirA/dirB/dir2")
    val featureFileAB2 = createFile("dirA/dirB/dir2/fileAB2.feature")
    val metaFileB2    = createFile("dirA/dirB/dir2/file.meta")
    val dirD           =  createDir("dirD")
    val featureFile1D  = createFile("dirD/file1D.feature")
    val featureFile2D  = createFile("dirD/file2D.feature")
    val dirDE          =  createDir("dirD/dirE")
    val metaFileDE     = createFile("dirD/dirE/file.meta")
    val featureFileDE  = createFile("dirD/dirE/fileDE.feature")
    val dirF           =  createDir("dirF")
    createFile("dirF/file.meta")

    val suiteStream = featureStream.readAll(List(dirA, dirAB, dirAB1, dirAB2, featureFileAB2, dirD, featureFile2D, dirDE, dirF), None)
    val suites = suiteStream.iterator

    // dirA suite
    var suite = suites.next()
    var units = suite.toList.sortBy(_.featureFile).iterator
    var unit = units.next()
    assertFile(featureFileAB1, unit.featureFile)
    assertMetaFiles(List(metaFileA, metaFileB), unit.metaFiles)
    unit.dataRecord should be (None)
    unit = units.next()
    assertFile(featureFileAB2, unit.featureFile)
    assertMetaFiles(List(metaFileA, metaFileB, metaFileB2), unit.metaFiles)
    unit.dataRecord should be (None)
    unit = units.next()
    assertFile(featureFileAB, unit.featureFile)
    assertMetaFiles(List(metaFileA, metaFileB), unit.metaFiles)
    unit.dataRecord should be (None)
    unit = units.next()
    assertFile(featureFileA, unit.featureFile)
    assertMetaFiles(List(metaFileA), unit.metaFiles)
    unit.dataRecord should be (None)
    assertEndOfStream(() => units.next())

    // dirAB suite
    suite = suites.next()
    units = suite.toList.sortBy(_.featureFile).iterator
    unit = units.next()
    assertFile(featureFileAB1, unit.featureFile)
    assertMetaFiles(List(metaFileB), unit.metaFiles)
    unit.dataRecord should be (None)
    unit = units.next()
    assertFile(featureFileAB2, unit.featureFile)
    assertMetaFiles(List(metaFileB, metaFileB2), unit.metaFiles)
    unit.dataRecord should be (None)
    unit = units.next()
    assertFile(featureFileAB, unit.featureFile)
    assertMetaFiles(List(metaFileB), unit.metaFiles)
    unit.dataRecord should be (None)
    assertEndOfStream(() => units.next())

    // dir AB1 suite
    suite = suites.next()
    units = suite.toList.sortBy(_.featureFile).iterator
    unit = units.next()
    assertFile(featureFileAB1, unit.featureFile)
    assertMetaFiles(List(metaFileA, metaFileB), unit.metaFiles)
    unit.dataRecord should be (None)
    assertEndOfStream(() => units.next())

    // dir AB2 suite
    suite = suites.next()
    units = suite.toList.sortBy(_.featureFile).iterator
    unit = units.next()
    assertFile(featureFileAB2, unit.featureFile)
    assertMetaFiles(List(metaFileB2), unit.metaFiles)
    unit.dataRecord should be (None)
    assertEndOfStream(() => units.next())

    // featureFile AB2 suite
    suite = suites.next()
    units = suite.toList.sortBy(_.featureFile).iterator
    unit = units.next()
    assertFile(featureFileAB2, unit.featureFile)
    assertMetaFiles(List(metaFileA, metaFileB, metaFileB2), unit.metaFiles)
    unit.dataRecord should be (None)
    assertEndOfStream(() => units.next())

    // dir D suite
    suite = suites.next()
    units = suite.toList.sortBy(_.featureFile).iterator
    unit = units.next()
    assertFile(featureFileDE, unit.featureFile)
    assertMetaFiles(List(metaFileDE), unit.metaFiles)
    unit.dataRecord should be (None)
    unit = units.next()
    assertFile(featureFile1D, unit.featureFile)
    assertMetaFiles(Nil, unit.metaFiles)
    unit.dataRecord should be (None)
    unit = units.next()
    assertFile(featureFile2D, unit.featureFile)
    assertMetaFiles(Nil, unit.metaFiles)
    unit.dataRecord should be (None)
    assertEndOfStream(() => units.next())

    // featureFile 2D suite
    suite = suites.next()
    units = suite.toList.sortBy(_.featureFile).iterator
    unit = units.next()
    assertFile(featureFile2D, unit.featureFile)
    assertMetaFiles(Nil, unit.metaFiles)
    unit.dataRecord should be (None)
    assertEndOfStream(() => units.next())

    // dir DE suite
    suite = suites.next()
    units = suite.toList.sortBy(_.featureFile).iterator
    unit = units.next()
    assertFile(featureFileDE, unit.featureFile)
    assertMetaFiles(List(metaFileDE), unit.metaFiles)
    unit.dataRecord should be (None)
    assertEndOfStream(() => units.next())

    // dir F suite
    suite = suites.next()
    units = suite.toList.sortBy(_.featureFile).iterator
    assertEndOfStream(() => units.next())

  }

  "multi suite with associative meta stream" should "be read in correctly" in {

    val dirJ           = createDir("dirJ")
    val metaTopJ       = createFile("dirJ/topJ.meta")
    val metaFileJ      = createFile("dirJ/fileJ.meta")
    val featureFileJ   = createFile("dirJ/fileJ.feature")
    val metaFileJJ     = createFile("dirJ/fileJJ.meta")
    val featureFileJJ  = createFile("dirJ/fileJJ.feature")
    val dirJB          =  createDir("dirJ/dirB")
    val metaFileJB     = createFile("dirJ/dirB/fileJB.meta")
    val featureFileJB  = createFile("dirJ/dirB/fileJB.feature")
    val dirJB1         =  createDir("dirJ/dirB/dir1")
    val featureFileJB1 = createFile("dirJ/dirB/dir1/fileJB1.feature")
    val dirJB2         =  createDir("dirJ/dirB/dir2")
    val featureFileJB2 = createFile("dirJ/dirB/dir2/fileJB2.feature")
    val metaFileJB2    = createFile("dirJ/dirB/dir2/fileJB2.meta")
    val dirK           =  createDir("dirK")
    val featureFile1K  = createFile("dirK/file1K.feature")
    val featureFile2K  = createFile("dirK/file2K.feature")
    val dirKE          =  createDir("dirK/dirE")
    val metaFileKE     = createFile("dirK/dirE/fileKE.meta")
    val featureFileKE  = createFile("dirK/dirE/fileKE.feature")
    val dirL           =  createDir("dirL")
    createFile("dirL/fileL.meta")

      val suiteStream = featureStream.readAll(List(dirJ, dirJB, dirJB1, dirJB2, featureFileJB2, dirK, featureFile2K, dirKE, dirL), None)
    val suites = suiteStream.iterator

    // dirJ suite
    var suite = suites.next()
    var units = suite.toList.sortBy(_.featureFile).iterator
    var unit = units.next()
    assertFile(featureFileJB1, unit.featureFile)
    assertMetaFiles(List(metaTopJ, metaFileJ, metaFileJJ, metaFileJB), unit.metaFiles)
    unit.dataRecord should be (None)
    unit = units.next()
    assertFile(featureFileJB2, unit.featureFile)
    assertMetaFiles(List(metaFileJB2), unit.metaFiles)
    unit.dataRecord should be (None)
    unit = units.next()
    assertFile(featureFileJB, unit.featureFile)
    assertMetaFiles(List(metaFileJB), unit.metaFiles)
    unit.dataRecord should be (None)
    unit = units.next()
    assertFile(featureFileJ, unit.featureFile)
    assertMetaFiles(List(metaFileJ), unit.metaFiles)
    unit.dataRecord should be (None)
    unit = units.next()
    assertFile(featureFileJJ, unit.featureFile)
    assertMetaFiles(List(metaFileJJ), unit.metaFiles)
    unit.dataRecord should be (None)
    assertEndOfStream(() => units.next())

    // dirJB suite
    suite = suites.next()
    units = suite.toList.sortBy(_.featureFile).iterator
    unit = units.next()
    assertFile(featureFileJB1, unit.featureFile)
    assertMetaFiles(List(metaTopJ, metaFileJ, metaFileJB), unit.metaFiles)
    unit.dataRecord should be (None)
    unit = units.next()
    assertFile(featureFileJB2, unit.featureFile)
    assertMetaFiles(List(metaFileJB2), unit.metaFiles)
    unit.dataRecord should be (None)
    unit = units.next()
    assertFile(featureFileJB, unit.featureFile)
    assertMetaFiles(List(metaFileJB), unit.metaFiles)
    unit.dataRecord should be (None)
    assertEndOfStream(() => units.next())

    // dir JB1 suite
    suite = suites.next()
    units = suite.toList.sortBy(_.featureFile).iterator
    unit = units.next()
    assertFile(featureFileJB1, unit.featureFile)
    assertMetaFiles(List(metaTopJ, metaFileJ, metaFileJB), unit.metaFiles)
    unit.dataRecord should be (None)
    assertEndOfStream(() => units.next())

    // dir JB2 suite
    suite = suites.next()
    units = suite.toList.sortBy(_.featureFile).iterator
    unit = units.next()
    assertFile(featureFileJB2, unit.featureFile)
    assertMetaFiles(List(metaFileJB2), unit.metaFiles)
    unit.dataRecord should be (None)
    assertEndOfStream(() => units.next())

    // featureFile JB2 suite
    suite = suites.next()
    units = suite.toList.sortBy(_.featureFile).iterator
    unit = units.next()
    assertFile(featureFileJB2, unit.featureFile)
    assertMetaFiles(List(metaFileJB2), unit.metaFiles)
    unit.dataRecord should be (None)
    assertEndOfStream(() => units.next())

    // dir K suite
    suite = suites.next()
    units = suite.toList.sortBy(_.featureFile).iterator
    unit = units.next()
    assertFile(featureFileKE, unit.featureFile)
    assertMetaFiles(List(metaFileKE), unit.metaFiles)
    unit.dataRecord should be (None)
    unit = units.next()
    assertFile(featureFile1K, unit.featureFile)
    assertMetaFiles(Nil, unit.metaFiles)
    unit.dataRecord should be (None)
    unit = units.next()
    assertFile(featureFile2K, unit.featureFile)
    assertMetaFiles(Nil, unit.metaFiles)
    unit.dataRecord should be (None)
    assertEndOfStream(() => units.next())

    // featureFile 2K suite
    suite = suites.next()
    units = suite.toList.sortBy(_.featureFile).iterator
    unit = units.next()
    assertFile(featureFile2K, unit.featureFile)
    assertMetaFiles(Nil, unit.metaFiles)
    unit.dataRecord should be (None)
    assertEndOfStream(() => units.next())

    // dir KE suite
    suite = suites.next()
    units = suite.toList.sortBy(_.featureFile).iterator
    unit = units.next()
    assertFile(featureFileKE, unit.featureFile)
    assertMetaFiles(List(metaFileKE), unit.metaFiles)
    unit.dataRecord should be (None)
    assertEndOfStream(() => units.next())

    // dir F suite
    suite = suites.next()
    units = suite.toList.sortBy(_.featureFile).iterator
    assertEndOfStream(() => units.next())

  }

  private def createFile(filepath: String): File = {
    val file = new File(rootDir.getPath + File.separator + filepath.replace('/', File.separatorChar))
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

  private def assertFile(expected: File, actual: File): Unit = {
    actual.getPath.startsWith("target") should be (true)
    actual.getPath should be (expected.getPath)
  }

  private def assertMetaFiles(expecteds: List[File], actuals: List[File]): Unit = {
    expecteds zip actuals foreach { case (expected, actual) =>
      val path = actual.getPath
      path.startsWith("target") should be (true)
      path should be (expected.getPath)
    }
  }

  private def assertEndOfStream(next: () => Unit): Unit = {
    intercept[NoSuchElementException] {
      next()
    }
  }

}
