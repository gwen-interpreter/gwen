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
import org.mockito.Mockito.never
import org.mockito.Mockito.times
import org.mockito.Mockito.verify
import org.mockito.Mockito.when
import org.scalatest.Matchers
import org.scalatest.mock.MockitoSugar
import gwen.dsl.Failed
import gwen.dsl.Feature
import gwen.dsl.FeatureSpec
import gwen.dsl.Passed
import gwen.dsl.Scenario
import gwen.dsl.StatusKeyword
import gwen.dsl.Step
import gwen.dsl.StepKeyword
import gwen.dsl.Tag
import gwen.UserOverrides
import org.scalatest.FlatSpec
import gwen.report.ReportFormat

class GwenLauncherTest extends FlatSpec with Matchers with MockitoSugar {
  
  val rootDir = new File("target" + File.separator + "GwenLauncherTest")
  Path(rootDir).createDirectory()
  
  val feature = new FeatureSpec(
      Feature("test-feature", Nil), 
      None, 
      List(Scenario(Set[Tag](), "scenario1", Nil, None, List(Step(StepKeyword.Given, "I am a test", Passed(10)))))
  )
  
  private def launcher(mockInterpreter: GwenInterpreter[EnvContext]) = {
    new GwenLauncher(mockInterpreter)
  }
  
  "test launcher with no given env context" should "create one and close it but not reset it" in {
    
    val dir1 = createDir("dir1");
    val feature1 = createFile("dir1/file1.feature");
    
    val options = GwenOptions(features = List(dir1), parallel = true)
    
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockEnv = mock[EnvContext]
    
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv);
    when(mockInterpreter.interpretFeature(new FeatureUnit(feature1, UserOverrides.addUserMeta(Nil), None), Nil, mockEnv)).thenReturn(List(feature))
    
    val evalStatus = launcher(mockInterpreter).run(options)
    
    verify(mockInterpreter, never()).reset(mockEnv)
    verify(mockInterpreter).close(mockEnv)
    
    evalStatus should be (Passed(10))
    
  }
  
  "test launcher with given env context" should "reset it when done but not close it" in {
    
    val dir2 = createDir("dir2");
    val feature2 = createFile("dir2/file2.feature");
    
    val options = GwenOptions(features = List(dir2))
    
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockEnv = mock[EnvContext]
    
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv);
    when(mockInterpreter.interpretFeature(new FeatureUnit(feature2, UserOverrides.addUserMeta(Nil), None), Nil, mockEnv)).thenReturn(List(feature))
    
    val evalStatus = launcher(mockInterpreter).run(options, Some(mockEnv))
    
    verify(mockInterpreter).reset(mockEnv)
    verify(mockInterpreter, never()).close(mockEnv)
    
    evalStatus should be (Passed(10))
    
  }
  
  "test launcher with given meta" should "load it" in {
    
    val dir3 = createDir("dir3");
    val feature3 = createFile("dir3/file3.feature");
    val meta3 = createFile("dir3/file3.meta");
    
    val options = GwenOptions(features = List(dir3), parallel = true)
    
    val meta = new FeatureSpec(
      Feature("meta feature", Nil), 
      None, 
      List(Scenario(Set[Tag](), "scenario1", Nil, None, List(Step(StepKeyword.Given, "I am a meta step", Passed(10)))))
    )
    
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockEnv = mock[EnvContext]
    val metas = UserOverrides.addUserMeta(List(meta3))
    
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv);
    when(mockInterpreter.interpretFeature(new FeatureUnit(feature3, metas, None), Nil, mockEnv)).thenReturn(List(feature))
    
    val evalStatus = launcher(mockInterpreter).run(options, Some(mockEnv))
    
    verify(mockInterpreter).reset(mockEnv)
    verify(mockInterpreter, never()).close(mockEnv)
    
    evalStatus should be (Passed(10))
    
  }
  
  "test launcher with duplicate meta" should "should not load duplicate" in {
    
    val dir31 = createDir("dir31")
    val dirmeta32 = createDir("dirmeta32");
    val feature3 = createFile("dir31/file3.feature");
    val meta31 = createFile("dir31/file31.meta");
    val meta32 = createFile("dirmeta32/file32.meta");
    
    val options = GwenOptions(features = List(dir31), parallel = true, metaFiles=List(meta31, meta32))
    
    val meta = new FeatureSpec(
      Feature("meta feature", Nil), 
      None, 
      List(Scenario(Set[Tag](), "scenario1", Nil, None, List(Step(StepKeyword.Given, "I am a meta step", Passed(10)))))
    )
    
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockEnv = mock[EnvContext]
    
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv);
    when(mockInterpreter.interpretFeature(new FeatureUnit(feature3, UserOverrides.addUserMeta(options.metaFiles), None), Nil, mockEnv)).thenReturn(List(feature))
    
    val evalStatus = launcher(mockInterpreter).run(options, Some(mockEnv))
    
    verify(mockInterpreter).reset(mockEnv)
    verify(mockInterpreter, never()).close(mockEnv)
    
    evalStatus should be (Passed(10))
    
  }
  
  "test launcher with given meta that throws exception in interactive mode" should "error" in {
    
    val dir4 = createDir("dir4");
    val feature4 = createFile("dir4/file4.feature");
    val meta4 = createFile("dir4/file4.meta");
    
    val meta = new FeatureSpec(
      Feature("meta feature", Nil), 
      None, 
      List(Scenario(Set[Tag](), "scenario1", Nil, None, List(Step(StepKeyword.Given, "I am a meta st ep", Failed(5, new Exception("failed"))))))
    )
    
    val options = GwenOptions(batch = false, features = List(dir4))
    
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockEnv = mock[EnvContext]
    
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv);
    when(mockInterpreter.interpretFeature(new FeatureUnit(feature4, UserOverrides.addUserMeta(List(meta4)), None), Nil, mockEnv)).thenThrow(new RuntimeException("meta error (don't be alarmed, this is a negative test)"))
    
    try {
      launcher(mockInterpreter).run(options, Some(mockEnv))
      fail("Exception expected")
    } catch {
      case e: Throwable => e.getMessage() should not be (null)
    } finally {
      verify(mockInterpreter).reset(mockEnv)
      verify(mockInterpreter, never()).close(mockEnv)
    }
    
  }
  
  "test launcher with given meta that throws exception in batch mode" should "return fail status" in {
    
    val dir5 = createDir("dir5");
    val feature5 = createFile("dir5/file5.feature");
    val meta5 = createFile("dir5/file5.meta");
    
    val options = GwenOptions(batch = true, parallel = true, features = List(dir5))
    
    val failedStatus = Failed(5, new Exception("failed"))
    val meta = new FeatureSpec(
      Feature("meta feature", Nil), 
      None, 
      List(Scenario(Set[Tag](), "scenario1", Nil, None, List(Step(StepKeyword.Given, "I am a meta st ep", failedStatus))))
    )
    
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockEnv = mock[EnvContext]
    
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv);
    when(mockInterpreter.interpretFeature(new FeatureUnit(feature5, UserOverrides.addUserMeta(List(meta5)), None), Nil, mockEnv)).thenThrow(new RuntimeException("meta error (don't be alarmed, this is a negative test)"))
    
    val evalStatus = launcher(mockInterpreter).run(options, Some(mockEnv))

    verify(mockInterpreter).reset(mockEnv)
    verify(mockInterpreter, never()).close(mockEnv)
    
    evalStatus.status should be (StatusKeyword.Failed)
    
  }
  
  "test launcher with html reporting" should "generate html reports" in {
    
    val dir6 = createDir("dir6");
    val feature6a = createFile("dir6/file6a.feature");
    val feature6b = createFile("dir6/file6b.feature");
    val dir7 = createDir("dir7");
    val feature7a = createFile("dir7/file7a.feature");
    val reportDir = createDir("report");
    
    val options = GwenOptions(features = List(dir6, feature7a), parallel = true, reportDir = Some(reportDir), reportFormats = List(ReportFormat.html))
    
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockEnv = mock[EnvContext]
    
    val feature6A = new FeatureSpec(
      Feature("test-feature-6a", Nil), 
      None, 
      List(Scenario(Set[Tag](), "scenario6A", Nil, None, List(Step(StepKeyword.Given, "I am a test 6A", Passed(1000))))),
      Some(feature6a)
    )
    val feature6B = new FeatureSpec(
      Feature("test-feature-6b", Nil), 
      None, 
      List(Scenario(Set[Tag](), "scenario6B", Nil, None, List(Step(StepKeyword.Given, "I am a test 6B", Passed(2000))))),
      Some(feature6b)
    )
    
    val feature7A = new FeatureSpec(
      Feature("test-feature-7a", Nil), 
      None, 
      List(Scenario(Set[Tag](), "scenario7A", Nil, None, List(Step(StepKeyword.Given, "I am a test 7A", Passed(3000))))),
      Some(feature7a)
    )
    
    when(mockInterpreter.implName).thenReturn("gwen")
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv);
    when(mockInterpreter.interpretFeature(new FeatureUnit(feature6a, UserOverrides.addUserMeta(Nil), None), Nil, mockEnv)).thenReturn(List(feature6A))
    when(mockInterpreter.interpretFeature(new FeatureUnit(feature6b, UserOverrides.addUserMeta(Nil), None), Nil, mockEnv)).thenReturn(List(feature6B))
    when(mockInterpreter.interpretFeature(new FeatureUnit(feature7a, UserOverrides.addUserMeta(Nil), None), Nil, mockEnv)).thenReturn(List(feature7A))
    
    val evalStatus = launcher(mockInterpreter).run(options)
    
    verify(mockInterpreter, never()).reset(mockEnv)
    verify(mockInterpreter, times(3)).close(mockEnv)
    
    evalStatus should be (Passed(6000))
    
    new File(reportDir.getPath() + File.separator + "html" + File.separator + "target-GwenLauncherTest-dir6" + File.separator + "file6a", "file6a.feature.html").exists should be (true)
    new File(reportDir.getPath() + File.separator + "html" + File.separator + "target-GwenLauncherTest-dir6" + File.separator + "file6b", "file6b.feature.html").exists should be (true)
    new File(reportDir.getPath() + File.separator + "html" + File.separator + "target-GwenLauncherTest-dir7" + File.separator + "file7a", "file7a.feature.html").exists should be (true)
    new File(reportDir.getPath() + File.separator + "html", "feature-summary.html").exists should be (true)
    new File(reportDir, "index.html").exists should be (true)
    
  }
  
  "test launcher with filter tags" should "apply the filters at interpreter level" in {
    
    val dir8 = createDir("dir8");
    val feature8 = createFile("dir8/file8.feature");
    val tagFilters = List((Tag("wip"), true))
    
    val options = GwenOptions(features = List(dir8), parallel = true, tags = tagFilters)
    
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockEnv = mock[EnvContext]
    
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv);
    when(mockInterpreter.interpretFeature(new FeatureUnit(feature8, UserOverrides.addUserMeta(Nil), None), tagFilters, mockEnv)).thenReturn(List(feature))
    
    val evalStatus = launcher(mockInterpreter).run(options)
    
    verify(mockInterpreter, never()).reset(mockEnv)
    verify(mockInterpreter).close(mockEnv)
    
    evalStatus should be (Passed(10))
    
  }
  
  private def createDir(dirname: String): File = {
    val dir = new File(rootDir, dirname)
    val path = Path(dir)
    path.deleteRecursively()
    path.createDirectory()
    dir
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
  
}