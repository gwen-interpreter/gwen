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

package gwen.core.eval

import gwen.core._
import gwen.core.eval._
import gwen.core.node._
import gwen.core.node.gherkin._
import gwen.core.report.ReportFormat
import gwen.core.result.SpecResult
import gwen.core.state.EnvState
import gwen.core.state.StateLevel
import gwen.core.status._

import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.same
import org.mockito.Mockito.never
import org.mockito.Mockito.spy
import org.mockito.Mockito.times
import org.mockito.Mockito.verify
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar

import scala.util.chaining._

import java.io.File
import java.{util => ju}
import org.scalatest.matchers.should.Matchers

class GwenLauncherTest extends BaseTest with Matchers with MockitoSugar with TestModel {

  val rootDir = new File("target" + File.separator + "GwenLauncherTest") tap { _.mkdirs() }

  val feature = new Spec(
      Feature(None, "test-feature", Nil),
      None,
      List(Scenario(List[Tag](), "scenario1", Nil, None, List(Step(StepKeyword.Given.toString, "I am a test", Passed(10))))),
      Nil,
      Nil
  )
  val result = new SpecResult(feature, None, Nil, new ju.Date(), new ju.Date())

  private def launcher(mockEngine: EvalEngine[EvalContext]) = {
    new GwenLauncher(mockEngine)
  }

  "test launcher with no given env context" should "create one and close it but not reset it" in {

    val dir1 = createDir("dir1")
    createFile("dir1/file1.feature")

    val options = GwenOptions(features = List(dir1), parallel = true)

    val mockEngine = mock[EvalEngine[EvalContext]]
    val mockCtx = spy(new EvalContext(options, EnvState()))

    when(mockEngine.implHome).thenReturn("https://github.com/gwen-interpreter/gwen")
    when(mockEngine.releaseNotesUrl).thenReturn(Some("https://github.com/gwen-interpreter/gwen/releases/latest"))
    when(mockEngine.implName).thenReturn("gwen")
    when(mockEngine.implVersion).thenReturn("1.0.0")
    when(mockEngine.gwenHome).thenReturn("https://gweninterpreter.org")
    when(mockEngine.init(same(options), any[EnvState])).thenReturn(mockCtx)
    when(mockEngine.evaluateUnit(any[FeatureUnit], any[EvalContext])).thenReturn(Some(result))

    val evalStatus = launcher(mockEngine).run(options)

    verify(mockCtx, never()).reset(StateLevel.feature)
    verify(mockCtx).close()

    evalStatus should be (Passed(10))

  }

  "test launcher with given env context" should "reset it when done but not close it" in {

    val dir2 = createDir("dir2")
    createFile("dir2/file2.feature")

    val options = GwenOptions(features = List(dir2))

    val mockEngine = mock[EvalEngine[EvalContext]]
    val mockCtx = spy(new EvalContext(options, EnvState()))

    when(mockEngine.implHome).thenReturn("https://github.com/gwen-interpreter/gwen")
    when(mockEngine.releaseNotesUrl).thenReturn(Some("https://github.com/gwen-interpreter/gwen/releases/latest"))
    when(mockEngine.implName).thenReturn("gwen")
    when(mockEngine.implVersion).thenReturn("1.0.0")
    when(mockEngine.gwenHome).thenReturn("https://gweninterpreter.org")
    when(mockEngine.init(same(options), any[EnvState])).thenReturn(mockCtx)
    when(mockEngine.evaluateUnit(any[FeatureUnit], any[EvalContext])).thenReturn(Some(result))

    val evalStatus = launcher(mockEngine).run(options, Some(mockCtx))

    verify(mockCtx).reset(StateLevel.feature)
    verify(mockCtx, never()).close()

    evalStatus should be (Passed(10))

  }

  "test launcher with given meta" should "load it" in {

    val dir3 = createDir("dir3")
    createFile("dir3/file3.feature")
    createFile("dir3/file3.meta")

    val options = GwenOptions(features = List(dir3), parallel = true)

    val mockEngine = mock[EvalEngine[EvalContext]]
    val mockCtx = spy(new EvalContext(options, EnvState()))

    when(mockEngine.implHome).thenReturn("https://github.com/gwen-interpreter/gwen")
    when(mockEngine.releaseNotesUrl).thenReturn(Some("https://github.com/gwen-interpreter/gwen/releases/latest"))
    when(mockEngine.implName).thenReturn("gwen")
    when(mockEngine.implVersion).thenReturn("1.0.0")
    when(mockEngine.gwenHome).thenReturn("https://gweninterpreter.org")
    when(mockEngine.init(same(options), any[EnvState])).thenReturn(mockCtx)
    when(mockEngine.evaluateUnit(any[FeatureUnit], any[EvalContext])).thenReturn(Some(result))

    val evalStatus = launcher(mockEngine).run(options, Some(mockCtx))

    verify(mockCtx).reset(StateLevel.feature)
    verify(mockCtx, never()).close()

    evalStatus should be (Passed(10))

  }

  "test launcher with given meta dir" should "load it" in {

    val dir3 = createDir("dir3")
    createFile("dir3/file3.feature")
    val metadir = createDir("dir3/meta")
    createFile("dir3/meta/file3.meta")

    val options = GwenOptions(features = List(dir3), parallel = true, metas = List(metadir))

    val mockEngine = mock[EvalEngine[EvalContext]]
    val mockCtx = spy(new EvalContext(options, EnvState()))

    when(mockEngine.implHome).thenReturn("https://github.com/gwen-interpreter/gwen")
    when(mockEngine.releaseNotesUrl).thenReturn(Some("https://github.com/gwen-interpreter/gwen/releases/latest"))
    when(mockEngine.implName).thenReturn("gwen")
    when(mockEngine.implVersion).thenReturn("1.0.0")
    when(mockEngine.gwenHome).thenReturn("https://gweninterpreter.org")
    when(mockEngine.init(same(options), any[EnvState])).thenReturn(mockCtx)
    when(mockEngine.evaluateUnit(any[FeatureUnit], any[EvalContext])).thenReturn(Some(result))

    val evalStatus = launcher(mockEngine).run(options, Some(mockCtx))

    verify(mockCtx).reset(StateLevel.feature)
    verify(mockCtx, never()).close()

    evalStatus should be (Passed(10))

  }

  "test launcher with duplicate meta" should "should not load duplicate" in {

    val dir31 = createDir("dir31")
    createDir("dirmeta32")
    createFile("dir31/file3.feature")
    val meta31 = createFile("dir31/file31.meta")
    val meta32 = createFile("dirmeta32/file32.meta")

    val options = GwenOptions(features = List(dir31), parallel = true, metas=List(meta31, meta32))

    val mockEngine = mock[EvalEngine[EvalContext]]
    val mockCtx = spy(new EvalContext(options, EnvState()))

    when(mockEngine.implHome).thenReturn("https://github.com/gwen-interpreter/gwen")
    when(mockEngine.releaseNotesUrl).thenReturn(Some("https://github.com/gwen-interpreter/gwen/releases/latest"))
    when(mockEngine.implName).thenReturn("gwen")
    when(mockEngine.implVersion).thenReturn("1.0.0")
    when(mockEngine.gwenHome).thenReturn("https://gweninterpreter.org")
    when(mockEngine.init(same(options), any[EnvState])).thenReturn(mockCtx)
    when(mockEngine.evaluateUnit(any[FeatureUnit], any[EvalContext])).thenReturn(Some(result))

    val evalStatus = launcher(mockEngine).run(options, Some(mockCtx))

    verify(mockCtx).reset(StateLevel.feature)
    verify(mockCtx, never()).close()

    evalStatus should be (Passed(10))

  }

  "test launcher with given meta that throws exception in interactive mode" should "error" in {

    val dir4 = createDir("dir4")
    createFile("dir4/file4.feature")
    createFile("dir4/file4.meta")

    val options = GwenOptions(batch = false, features = List(dir4))

    val mockEngine = mock[EvalEngine[EvalContext]]
    val mockCtx = spy(new EvalContext(options, EnvState()))

    when(mockEngine.implHome).thenReturn("https://github.com/gwen-interpreter/gwen")
    when(mockEngine.releaseNotesUrl).thenReturn(Some("https://github.com/gwen-interpreter/gwen/releases/latest"))
    when(mockEngine.implName).thenReturn("gwen")
    when(mockEngine.implVersion).thenReturn("1.0.0")
    when(mockEngine.gwenHome).thenReturn("https://gweninterpreter.org")
    when(mockEngine.init(same(options), any[EnvState])).thenReturn(mockCtx)
    when(mockEngine.evaluateUnit(any[FeatureUnit], any[EvalContext])).thenThrow(new RuntimeException("meta error (don't be alarmed, this is a negative test)"))

    try {
      launcher(mockEngine).run(options, Some(mockCtx))
      fail("Exception expected")
    } catch {
      case e: Throwable => e.getMessage() should not be (null)
    } finally {
      verify(mockCtx).reset(StateLevel.feature)
      verify(mockCtx, never()).close()
    }

  }

  "test launcher with given meta that throws exception in batch mode" should "return fail status" in {

    val dir5 = createDir("dir5")
    createFile("dir5/file5.feature")
    createFile("dir5/file5.meta")

    val options = GwenOptions(batch = true, parallel = true, features = List(dir5))

    val mockEngine = mock[EvalEngine[EvalContext]]
    val mockCtx = spy(new EvalContext(options, EnvState()))

    when(mockEngine.implHome).thenReturn("https://github.com/gwen-interpreter/gwen")
    when(mockEngine.releaseNotesUrl).thenReturn(Some("https://github.com/gwen-interpreter/gwen/releases/latest"))
    when(mockEngine.implName).thenReturn("gwen")
    when(mockEngine.implVersion).thenReturn("1.0.0")
    when(mockEngine.gwenHome).thenReturn("https://gweninterpreter.org")
    when(mockEngine.init(same(options), any[EnvState])).thenReturn(mockCtx)
    when(mockEngine.evaluateUnit(any[FeatureUnit], any[EvalContext])).thenThrow(new RuntimeException("meta error (don't be alarmed, this is a negative test)"))

    val evalStatus = launcher(mockEngine).run(options, Some(mockCtx))

    verify(mockCtx).reset(StateLevel.feature)
    verify(mockCtx, never()).close()

    evalStatus.keyword should be (StatusKeyword.Failed)

  }

  "test launcher with html reporting" should "generate html reports" in {

    val dir6 = createDir("dir6")
    createDir("dir7")
    val feature6a = createFile("dir6/file6a.feature")
    val feature6b = createFile("dir6/file6b.feature")
    val feature7a = createFile("dir7/file7a.feature")
    val reportDir = createDir("report")

    val options = GwenOptions(features = List(dir6, feature7a), parallel = true, reportDir = Some(reportDir), reportFormats = List(ReportFormat.html))

    val mockEngine = mock[EvalEngine[EvalContext]]
    val mockCtx = spy(new EvalContext(options, EnvState()))

    val feature6A = new Spec(
      Feature(Some(feature6a), "test-feature-6a", Nil),
      None,
      List(Scenario(List[Tag](), "scenario6A", Nil, None, List(Step(StepKeyword.Given.toString, "I am a test 6A", Passed(1000))))),
      Nil,
      Nil
    )
    val feature6B = new Spec(
      Feature(Some(feature6b), "test-feature-6b", Nil),
      None,
      List(Scenario(List[Tag](), "scenario6B", Nil, None, List(Step(StepKeyword.Given.toString, "I am a test 6B", Passed(2000))))),
      Nil,
      Nil
    )

    val feature7A = new Spec(
      Feature(Some(feature7a), "test-feature-7a", Nil),
      None,
      List(Scenario(List[Tag](), "scenario7A", Nil, None, List(Step(StepKeyword.Given.toString, "I am a test 7A", Passed(3000))))),
      Nil,
      Nil
    )

    when(mockEngine.implHome).thenReturn("https://github.com/gwen-interpreter/gwen")
    when(mockEngine.releaseNotesUrl).thenReturn(Some("https://github.com/gwen-interpreter/gwen/releases/latest"))
    when(mockEngine.implName).thenReturn("gwen")
    when(mockEngine.implVersion).thenReturn("1.0.0")
    when(mockEngine.gwenHome).thenReturn("https://gweninterpreter.org")
    when(mockEngine.init(same(options), any[EnvState])).thenReturn(mockCtx)
    when(mockEngine.evaluateUnit(any[FeatureUnit], any[EvalContext])).thenReturn(
      Some(new SpecResult(feature6A, None, Nil, new ju.Date(), new ju.Date())),
      Some(new SpecResult(feature6B, None, Nil, new ju.Date(), new ju.Date())),
      Some(new SpecResult(feature7A, None, Nil, new ju.Date(), new ju.Date())))

    val evalStatus = launcher(mockEngine).run(options)

    verify(mockCtx, never()).reset(StateLevel.feature)
    verify(mockCtx, times(3)).close()

    evalStatus should be (Passed(6000))

    new File(reportDir.getPath() + File.separator + "html" + File.separator + "target-GwenLauncherTest-dir6" + File.separator + "file6a", "file6a.feature.html").exists should be (true)
    new File(reportDir.getPath() + File.separator + "html" + File.separator + "target-GwenLauncherTest-dir6" + File.separator + "file6b", "file6b.feature.html").exists should be (true)
    new File(reportDir.getPath() + File.separator + "html" + File.separator + "target-GwenLauncherTest-dir7" + File.separator + "file7a", "file7a.feature.html").exists should be (true)
    new File(reportDir.getPath() + File.separator + "html", "index.html").exists should be (true)
    new File(reportDir, "index.html").exists should be (true)

  }

  "test launcher with filter tags" should "apply the filters at interpreter level" in {

    val dir8 = createDir("dir8")
    createFile("dir8/file8.feature")
    val tagFilters = List((Tag("@wip"), true))

    val options = GwenOptions(features = List(dir8), parallel = true, tags = tagFilters)

    val mockEngine = mock[EvalEngine[EvalContext]]
    val mockCtx = spy(new EvalContext(options, EnvState()))

    when(mockEngine.implHome).thenReturn("https://github.com/gwen-interpreter/gwen")
    when(mockEngine.releaseNotesUrl).thenReturn(Some("https://github.com/gwen-interpreter/gwen/releases/latest"))
    when(mockEngine.implName).thenReturn("gwen")
    when(mockEngine.implVersion).thenReturn("1.0.0")
    when(mockEngine.gwenHome).thenReturn("https://gweninterpreter.org")
    when(mockEngine.init(same(options), any[EnvState])).thenReturn(mockCtx)
    when(mockEngine.evaluateUnit(any[FeatureUnit], any[EvalContext])).thenReturn(Some(result))

    val evalStatus = launcher(mockEngine).run(options)

    verify(mockCtx, never()).reset(StateLevel.feature)
    verify(mockCtx).close()

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
