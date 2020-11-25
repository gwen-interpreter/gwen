/*
 * Copyright 2014-2017 Branko Juric, Brady Wood
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

import gwen._
import gwen.dsl._
import gwen.report.ReportFormat

import org.mockito.Mockito.never
import org.mockito.Mockito.times
import org.mockito.Mockito.verify
import org.mockito.Mockito.when
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.mockito.Matchers.any
import org.mockito.Matchers.same
import org.scalatestplus.mockito.MockitoSugar

import java.io.File
import java.{util => ju}

class GwenLauncherTest extends FlatSpec with Matchers with MockitoSugar with GwenTestModel {

  val rootDir = new File("target" + File.separator + "GwenLauncherTest") tap { _.mkdirs() }
  
  val feature = new FeatureSpec(
      Feature("test-feature", Nil), 
      None, 
      List(Scenario(List[Tag](), "scenario1", Nil, None, List(Step(StepKeyword.Given.toString, "I am a test", Passed(10))))),
      Nil,
      None,
      Nil
  )
  val featureResult = new FeatureResult(feature, None, Nil, new ju.Date(), new ju.Date())
  
  private def launcher(mockInterpreter: GwenInterpreter[EnvContext]) = {
    new GwenLauncher(mockInterpreter)
  }
  
  "test launcher with no given env context" should "create one and close it but not reset it" in {
    
    val dir1 = createDir("dir1")
    createFile("dir1/file1.feature")
    
    val options = GwenOptions(features = List(dir1), parallel = true)
    
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockEnv = mock[EnvContext]
    
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv)
    when(mockInterpreter.interpretFeature(any[FeatureUnit], same(Nil), same(mockEnv), any[ju.Date])).thenReturn(Some(featureResult))
    
    val evalStatus = launcher(mockInterpreter).run(options)
    
    verify(mockEnv, never()).reset(StateLevel.feature)
    verify(mockEnv).close()
    
    evalStatus should be (Passed(10))
    
  }
  
  "test launcher with given env context" should "reset it when done but not close it" in {
    
    val dir2 = createDir("dir2")
    createFile("dir2/file2.feature")
    
    val options = GwenOptions(features = List(dir2))
    
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockEnv = mock[EnvContext]
    
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv)
    when(mockInterpreter.interpretFeature(any[FeatureUnit], same(Nil), same(mockEnv), any[ju.Date])).thenReturn(Some(featureResult))
    
    val evalStatus = launcher(mockInterpreter).run(options, Some(mockEnv))
    
    verify(mockEnv).reset(StateLevel.feature)
    verify(mockEnv, never()).close()
    
    evalStatus should be (Passed(10))
    
  }
  
  "test launcher with given meta" should "load it" in {
    
    val dir3 = createDir("dir3")
    createFile("dir3/file3.feature")
    createFile("dir3/file3.meta")
    
    val options = GwenOptions(features = List(dir3), parallel = true)
    
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockEnv = mock[EnvContext]
    
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv)
    when(mockInterpreter.interpretFeature(any[FeatureUnit], same(Nil), same(mockEnv), any[ju.Date])).thenReturn(Some(featureResult))
    
    val evalStatus = launcher(mockInterpreter).run(options, Some(mockEnv))
    
    verify(mockEnv).reset(StateLevel.feature)
    verify(mockEnv, never()).close()
    
    evalStatus should be (Passed(10))
    
  }
  
  "test launcher with given meta dir" should "load it" in {
    
    val dir3 = createDir("dir3")
    createFile("dir3/file3.feature")
    val metadir = createDir("dir3/meta")
    createFile("dir3/meta/file3.meta")
    
    val options = GwenOptions(features = List(dir3), parallel = true, metas = List(metadir))
        
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockEnv = mock[EnvContext]
    
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv)
    when(mockInterpreter.interpretFeature(any[FeatureUnit], same(Nil), same(mockEnv), any[ju.Date])).thenReturn(Some(featureResult))
    
    val evalStatus = launcher(mockInterpreter).run(options, Some(mockEnv))
    
    verify(mockEnv).reset(StateLevel.feature)
    verify(mockEnv, never()).close()
    
    evalStatus should be (Passed(10))
    
  }
  
  "test launcher with duplicate meta" should "should not load duplicate" in {
    
    val dir31 = createDir("dir31")
    createDir("dirmeta32")
    createFile("dir31/file3.feature")
    val meta31 = createFile("dir31/file31.meta")
    val meta32 = createFile("dirmeta32/file32.meta")
    
    val options = GwenOptions(features = List(dir31), parallel = true, metas=List(meta31, meta32))
    
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockEnv = mock[EnvContext]
    
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv)
    when(mockInterpreter.interpretFeature(any[FeatureUnit], same(Nil), same(mockEnv), any[ju.Date])).thenReturn(Some(featureResult))
    
    val evalStatus = launcher(mockInterpreter).run(options, Some(mockEnv))
    
    verify(mockEnv).reset(StateLevel.feature)
    verify(mockEnv, never()).close()
    
    evalStatus should be (Passed(10))
    
  }
  
  "test launcher with given meta that throws exception in interactive mode" should "error" in {
    
    val dir4 = createDir("dir4")
    createFile("dir4/file4.feature")
    createFile("dir4/file4.meta")
    
    val options = GwenOptions(batch = false, features = List(dir4))
    
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockEnv = mock[EnvContext]
    
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv)
    when(mockInterpreter.interpretFeature(any[FeatureUnit], same(Nil), same(mockEnv), any[ju.Date])).thenThrow(new RuntimeException("meta error (don't be alarmed, this is a negative test)"))
    
    try {
      launcher(mockInterpreter).run(options, Some(mockEnv))
      fail("Exception expected")
    } catch {
      case e: Throwable => e.getMessage() should not be (null)
    } finally {
      verify(mockEnv).reset(StateLevel.feature)
      verify(mockEnv, never()).close()
    }
    
  }
  
  "test launcher with given meta that throws exception in batch mode" should "return fail status" in {
    
    val dir5 = createDir("dir5")
    createFile("dir5/file5.feature")
    createFile("dir5/file5.meta")
    
    val options = GwenOptions(batch = true, parallel = true, features = List(dir5))
        
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockEnv = mock[EnvContext]
    
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv)
    when(mockInterpreter.interpretFeature(any[FeatureUnit], same(Nil), same(mockEnv), any[ju.Date])).thenThrow(new RuntimeException("meta error (don't be alarmed, this is a negative test)"))
    
    val evalStatus = launcher(mockInterpreter).run(options, Some(mockEnv))

    verify(mockEnv).reset(StateLevel.feature)
    verify(mockEnv, never()).close()
    
    evalStatus.status should be (StatusKeyword.Failed)
    
  }
  
  "test launcher with html reporting" should "generate html reports" in {
    
    val dir6 = createDir("dir6")
    createDir("dir7")
    val feature6a = createFile("dir6/file6a.feature")
    val feature6b = createFile("dir6/file6b.feature")
    val feature7a = createFile("dir7/file7a.feature")
    val reportDir = createDir("report")
    
    val options = GwenOptions(features = List(dir6, feature7a), parallel = true, reportDir = Some(reportDir), reportFormats = List(ReportFormat.html))
    
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockEnv = mock[EnvContext]
    
    val feature6A = new FeatureSpec(
      Feature("test-feature-6a", Nil), 
      None, 
      List(Scenario(List[Tag](), "scenario6A", Nil, None, List(Step(StepKeyword.Given.toString, "I am a test 6A", Passed(1000))))),
      Nil,
      Some(feature6a),
      Nil
    )
    val feature6B = new FeatureSpec(
      Feature("test-feature-6b", Nil), 
      None, 
      List(Scenario(List[Tag](), "scenario6B", Nil, None, List(Step(StepKeyword.Given.toString, "I am a test 6B", Passed(2000))))),
      Nil,
      Some(feature6b),
      Nil
    )
    
    val feature7A = new FeatureSpec(
      Feature("test-feature-7a", Nil), 
      None, 
      List(Scenario(List[Tag](), "scenario7A", Nil, None, List(Step(StepKeyword.Given.toString, "I am a test 7A", Passed(3000))))),
      Nil,
      Some(feature7a),
      Nil
    )
    
    when(mockInterpreter.implName).thenReturn("gwen")
    when(mockInterpreter.implVersion).thenReturn("-SNAPSHOT")
    when(mockInterpreter.releaseNotesUrl).thenReturn(None)
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv)
    when(mockInterpreter.interpretFeature(any[FeatureUnit], same(Nil), same(mockEnv), any[ju.Date])).thenReturn(
      Some(new FeatureResult(feature6A, None, Nil, new ju.Date(), new ju.Date())),
      Some(new FeatureResult(feature6B, None, Nil, new ju.Date(), new ju.Date())),
      Some(new FeatureResult(feature7A, None, Nil, new ju.Date(), new ju.Date())))
    
    val evalStatus = launcher(mockInterpreter).run(options)
    
    verify(mockEnv, never()).reset(StateLevel.feature)
    verify(mockEnv, times(3)).close()
    
    evalStatus should be (Passed(6000))
    
    new File(reportDir.getPath() + File.separator + "html" + File.separator + "target-GwenLauncherTest-dir6" + File.separator + "file6a", "file6a.feature.html").exists should be (true)
    new File(reportDir.getPath() + File.separator + "html" + File.separator + "target-GwenLauncherTest-dir6" + File.separator + "file6b", "file6b.feature.html").exists should be (true)
    new File(reportDir.getPath() + File.separator + "html" + File.separator + "target-GwenLauncherTest-dir7" + File.separator + "file7a", "file7a.feature.html").exists should be (true)
    new File(reportDir.getPath() + File.separator + "html", "feature-summary.html").exists should be (true)
    new File(reportDir, "index.html").exists should be (true)
    
  }
  
  "test launcher with filter tags" should "apply the filters at interpreter level" in {
    
    val dir8 = createDir("dir8")
    createFile("dir8/file8.feature")
    val tagFilters = List((Tag("@wip"), true))
    
    val options = GwenOptions(features = List(dir8), parallel = true, tags = tagFilters)
    
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockEnv = mock[EnvContext]
    
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv)
    when(mockInterpreter.interpretFeature(any[FeatureUnit], same(tagFilters), same(mockEnv), any[ju.Date])).thenReturn(Some(featureResult))
    
    val evalStatus = launcher(mockInterpreter).run(options)
    
    verify(mockEnv, never()).reset(StateLevel.feature)
    verify(mockEnv).close()
    
    evalStatus should be (Passed(10))
    
  }
  
  private def createDir(dirname: String): File = {
    val dir = new File(rootDir, dirname)
    dir.deleteDir()
    dir.mkdirs()
    dir
  }
  
  private def createFile(filepath: String): File = {
    val file = new File(rootDir.getPath + File.separator + filepath.replace('/', File.separatorChar))
    if (file.exists) {
      file.delete()
    }
    file.getParentFile().mkdirs()
    file.createNewFile()
    file
  }
  
}