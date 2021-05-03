/*
 * Copyright 2014-2020 Branko Juric, Brady Wood
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

package gwen

import gwen.eval.EvalContext
import gwen.eval.EvalEngine
import gwen.eval.EvalEnvironment
import gwen.model._
import gwen.model.event.LifecycleEventDispatcher
import gwen.model.gherkin._
import gwen.model.state._

import scala.util.{Failure => TryFailure}
import scala.util.{Success => TrySuccess}

import org.mockito.Matchers.any
import org.mockito.Matchers.anyString
import org.mockito.Mockito.doReturn
import org.mockito.Mockito.never
import org.mockito.Mockito.spy
import org.mockito.Mockito.times
import org.mockito.Mockito.verify
import org.mockito.Mockito.when
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatestplus.mockito.MockitoSugar

import java.io.File
import java.io.FileWriter

class GwenInterpreter0Test extends FlatSpec with Matchers with MockitoSugar with TestModel {

  val rootDir: File = new File("target" + File.separator + "GwenInterpreterTest") tap { _.mkdirs() }
  
  val options = new GwenOptions()
  
  private def interpreter(mockCtx: EvalContext, mockLifecycle: LifecycleEventDispatcher) = {
    when(mockCtx.lifecycle).thenReturn(mockLifecycle)
    val engine = new EvalEngine[EvalContext] {
      override def init(options: GwenOptions, envOpt: Option[EvalEnvironment] = None): EvalContext = mockCtx
      override def evaluate(step: Step, ctx: EvalContext): Unit = { }
    }
    new GwenInterpreter(engine)
  }

  "initialise interpreter" should "create new env" in {
    val mockEnv = mock[EvalEnvironment]
    val mockCtx = spy(new EvalContext(options, mockEnv))
    val mockLifecycle = mock[LifecycleEventDispatcher]
    interpreter(mockCtx, mockLifecycle).init(options)
    verify(mockEnv, never()).close()
  }
  
  "interpreting a valid step" should "return success" in {
    val mockEnv = mock[EvalEnvironment]
    val mockCtx = spy(new EvalContext(GwenOptions(), mockEnv))
    val mockLifecycle = mock[LifecycleEventDispatcher]
    when(mockEnv.getStepDef("I am a valid step")).thenReturn(None)
    val step = Step(StepKeyword.Given.toString, "I am a valid step")
    doReturn(step).when(mockCtx).interpolateParams(any[Step])
    doReturn(step).when(mockCtx).interpolate(any[Step])
    doReturn(Step(step, Passed(1))).when(mockCtx).finaliseStep(any[Step])
    val result = interpreter(mockCtx, mockLifecycle).interpretStep("Given I am a valid step", mockCtx)
    result match {
      case TrySuccess(s) =>
        s.keyword should be (StepKeyword.Given.toString)
        s.name should be ("I am a valid step")
        s.evalStatus.status should be (StatusKeyword.Passed)
        verify(mockLifecycle).beforeStep(any[Identifiable], any[Step], any[ScopedDataStack])
        verify(mockLifecycle).afterStep(any[Step], any[ScopedDataStack])
      case TryFailure(err) =>
        err.printStackTrace()
        fail(s"success expected but got $err")
    }
  }

  "interpreting a valid step def" should "return success" in {
    val paramScope = new LocalDataStack()
    val mockEnv = mock[EvalEnvironment]
    val mockCtx = spy(new EvalContext(GwenOptions(), mockEnv))
    val mockLifecycle = mock[LifecycleEventDispatcher]
    val step1 = Step(StepKeyword.Given.toString, "I am a step in the stepdef")
    val step2 = Step(StepKeyword.Given.toString, "I am a valid stepdef")
    val stepdef = Scenario(List[Tag](Tag("@StepDef")), "I am a valid stepdef", Nil, None, List(step1))
    when(mockEnv.getStepDef("I am a valid stepdef")).thenReturn(Some((stepdef, Nil)))
    when(mockEnv.getStepDef("I am a step in the stepdef")).thenReturn(None)
    doReturn(step1).when(mockCtx).interpolateParams(any[Step])
    doReturn(step1).when(mockCtx).interpolate(any[Step])
    doReturn(Step(step1, Passed(1))).doReturn(Step(step2, Passed(1))).when(mockCtx).finaliseStep(any[Step])
    doReturn(step2).when(mockCtx).interpolateParams(any[Step])
    doReturn(step2).when(mockCtx).interpolate(any[Step])
    when(mockEnv.stepScope).thenReturn(paramScope)
    val result = interpreter(mockCtx, mockLifecycle).interpretStep("Given I am a valid stepdef", mockCtx)
    result match {
      case TrySuccess(step) =>
        step.keyword should be (StepKeyword.Given.toString)
        step.name should be ("I am a valid stepdef")
        step.evalStatus.status should be (StatusKeyword.Passed)
        verify(mockLifecycle, times(2)).beforeStep(any[Identifiable], any[Step], any[ScopedDataStack])
        verify(mockLifecycle, times(2)).afterStep(any[Step], any[ScopedDataStack])
      case TryFailure(err) =>
        fail(s"success expected but got $err")
    }
  }
  
  "interpreting an invalid step" should "return error" in {
    val mockEnv = mock[EvalEnvironment]
    val mockCtx = spy(new EvalContext(GwenOptions(), mockEnv))
    val mockLifecycle = mock[LifecycleEventDispatcher]
    val result = interpreter(mockCtx, mockLifecycle).interpretStep("Yes I am an invalid step", mockCtx)
    result match {
      case TrySuccess(_) =>
        fail("expected failure")
      case TryFailure(err) =>
        err.getMessage should be ("Gherkin syntax error [at line 1]: 'Given|When|Then|And|But <expression>' expected")
    }
  }
  
  "interpreting valid feature file with no meta" should "return success" in {
    val featureString = """
     Feature: Gwen
  Background: The observer
        Given I am an observer
         When I observe something
         Then it will become real
    Scenario: The butterfly effect
        Given a deterministic nonlinear system
         When a small change is initially applied
         Then a large change will eventually result"""
    
    val featureFile = writeToFile(featureString, createFile("test1.feature"))
    val mockEnv = mock[EvalEnvironment]
    val mockCtx = spy(new EvalContext(GwenOptions(), mockEnv))
    val mockTopScope = mock[TopScope]
    val mockLifecycle = mock[LifecycleEventDispatcher]
    when(mockEnv.getStepDef(anyString)).thenReturn(None)
    when(mockEnv.topScope).thenReturn(mockTopScope)
    when(mockEnv.specType).thenReturn(SpecType.Feature)
    val step1 = Step(StepKeyword.Given.toString, "I am an observer")
    val step2 = Step(StepKeyword.When.toString, "I observe something")
    val step3 = Step(StepKeyword.Then.toString, "it will become real")
    val step4 = Step(StepKeyword.Given.toString, "a deterministic nonlinear system")
    val step5 = Step(StepKeyword.When.toString, "a small change is initially applied")
    val step6 = Step(StepKeyword.Then.toString, "a large change will eventually result")
    doReturn(step1)
      .doReturn(step2)
      .doReturn(step3)
      .doReturn(step4)
      .doReturn(step5)
      .doReturn(step6)
      .when(mockCtx)
      .interpolateParams(any[Step])
    doReturn(step1)
      .doReturn(step2)
      .doReturn(step3)
      .doReturn(step4)
      .doReturn(step5)
      .doReturn(step6)
      .when(mockCtx)
      .interpolate(any[Step])
    doReturn(Step(step1, Passed(1)))
      .doReturn(Step(step2, Passed(1)))
      .doReturn(Step(step3, Passed(1)))
      .doReturn(Step(step4, Passed(1)))
      .doReturn(Step(step5, Passed(1)))
      .doReturn(Step(step6, Passed(1)))
      .when(mockCtx)
      .finaliseStep(any[Step])
    val result = interpreter(mockCtx, mockLifecycle).interpretUnit(FeatureUnit(Root, featureFile, Nil, None, new TagFilter(Nil)), mockCtx)
    result match {
      case Some(result) =>
        result.spec.evalStatus.status should be (StatusKeyword.Passed)
        verify(mockTopScope).set("gwen.feature.file.name", featureFile.getName)
        verify(mockTopScope).set("gwen.feature.file.path", featureFile.getPath)
        verify(mockTopScope).set("gwen.feature.file.absolutePath", featureFile.getAbsolutePath)
        verify(mockTopScope).set("gwen.feature.name", "Gwen")
        verify(mockTopScope).set("gwen.scenario.name", "The butterfly effect")
        verify(mockTopScope, never()).set("gwen.feature.scenario", "The observer")
        verify(mockLifecycle).beforeSpec(any[Identifiable], any[Spec], any[ScopedDataStack])
        verify(mockLifecycle).afterSpec(any[SpecResult], any[ScopedDataStack])
        verify(mockLifecycle).beforeBackground(any[Identifiable], any[Background], any[ScopedDataStack])
        verify(mockLifecycle).afterBackground(any[Background], any[ScopedDataStack])
        verify(mockLifecycle).beforeScenario(any[Identifiable], any[Scenario], any[ScopedDataStack])
        verify(mockLifecycle).afterScenario(any[Scenario], any[ScopedDataStack])
        verify(mockLifecycle, times(6)).beforeStep(any[Identifiable], any[Step], any[ScopedDataStack])
        verify(mockLifecycle, times(6)).afterStep(any[Step], any[ScopedDataStack])
      case None => 
        fail("Some(SpecResult) expected")
    }
  }
  
  "interpreting valid feature file with meta having stepdef" should "return success" in {
    val metaString = """
     Feature: Gwen meta
     @Stepdef
     @When
    Scenario: the butterfly flaps its wings
        Given a deterministic nonlinear system
         When a small change is initially applied
         Then a large change will eventually result"""
    val featureString = """
     Feature: Gwen
  Background: The observer
        Given I am an observer
         When I observe something
         Then it will become real
    Scenario: The butterfly effect
        Given there is order
         When the butterfly flaps its wings
         Then order is lost"""
    
    val paramScope = new LocalDataStack()
    val metaFile = writeToFile(metaString, createFile("test2.meta"))
    val featureFile = writeToFile(featureString, createFile("test2.feature"))
    val stepdef = Scenario(
      List[Tag](),
      "the butterfly flaps its wings", 
      Nil, 
      None, 
      List(
        Step(StepKeyword.Given.toString, "a deterministic nonlinear system"),
        Step(StepKeyword.When.toString, "a small change is initially applied"),
        Step(StepKeyword.Then.toString, "a large change will eventually result")))
    val mockEnv = mock[EvalEnvironment]
    val mockCtx = spy(new EvalContext(GwenOptions(), mockEnv))
    val mockTopScope = mock[TopScope]
    val mockLifecycle = mock[LifecycleEventDispatcher]
    when(mockEnv.getStepDef("I am an observer")).thenReturn(None)
    when(mockEnv.getStepDef("I observe something")).thenReturn(None)
    when(mockEnv.getStepDef("it will become real")).thenReturn(None)
    when(mockEnv.getStepDef("there is order")).thenReturn(None)
    when(mockEnv.getStepDef("the butterfly flaps its wings")).thenReturn(Some((stepdef, Nil)))
    when(mockEnv.getStepDef("a deterministic nonlinear system")).thenReturn(None)
    when(mockEnv.getStepDef("a small change is initially applied")).thenReturn(None)
    when(mockEnv.getStepDef("a large change will eventually result")).thenReturn(None)
    when(mockEnv.getStepDef("order is lost")).thenReturn(None)
    when(mockEnv.getStepDef("")).thenReturn(None)
    when(mockEnv.stepScope).thenReturn(paramScope)
    when(mockEnv.loadedMeta).thenReturn(Nil)
    when(mockEnv.topScope).thenReturn(mockTopScope)
    when(mockEnv.specType).thenReturn(SpecType.Feature)
    val step1 = Step(StepKeyword.Given.toString, "I am an observer")
    val step2 = Step(StepKeyword.When.toString, "I observe something")
    val step3 = Step(StepKeyword.Then.toString, "it will become real")
    val step4 = Step(StepKeyword.Given.toString, "there is order")
    val step5 = Step(StepKeyword.When.toString, "the butterfly flaps its wings")
    val step6 = Step(StepKeyword.Given.toString, "a deterministic nonlinear system")
    val step7 = Step(StepKeyword.When.toString, "a small change is initially applied")
    val step8 = Step(StepKeyword.Then.toString, "a large change will eventually result")
    val step9 = Step(StepKeyword.Then.toString, "order is lost")    
    doReturn(step1)
      .doReturn(step2)
      .doReturn(step3)
      .doReturn(step4)
      .doReturn(step5)
      .doReturn(step6)
      .doReturn(step7)
      .doReturn(step8)
      .doReturn(step9)
      .when(mockCtx)
      .interpolateParams(any[Step])
    doReturn(step1)
      .doReturn(step2)
      .doReturn(step3)
      .doReturn(step4)
      .doReturn(step5)
      .doReturn(step6)
      .doReturn(step7)
      .doReturn(step8)
      .doReturn(step9)
      .when(mockCtx)
      .interpolate(any[Step])
    doReturn(Step(step1, Passed(1)))
      .doReturn(Step(step2, Passed(1)))
      .doReturn(Step(step3, Passed(1)))
      .doReturn(Step(step4, Passed(1)))
      .doReturn(Step(step5, Passed(1)))
      .doReturn(Step(step6, Passed(1)))
      .doReturn(Step(step7, Passed(1)))
      .doReturn(Step(step8, Passed(1)))
      .doReturn(Step(step9, Passed(1)))
      .when(mockCtx)
      .finaliseStep(any[Step])
    val result = interpreter(mockCtx, mockLifecycle).interpretUnit(FeatureUnit(Root, featureFile, List(metaFile), None, new TagFilter(Nil)), mockCtx)
    result match {
      case Some(result) =>
        result.spec.evalStatus.status should be (StatusKeyword.Passed)
        verify(mockTopScope).set("gwen.feature.file.name", featureFile.getName)
        verify(mockTopScope).set("gwen.feature.file.path", featureFile.getPath)
        verify(mockTopScope).set("gwen.feature.file.absolutePath", featureFile.getAbsolutePath)
        verify(mockTopScope).set("gwen.feature.name", "Gwen")
        verify(mockTopScope).set("gwen.scenario.name", "The butterfly effect")
        verify(mockTopScope, never()).set("gwen.feature.name", "Gwen meta")
        verify(mockTopScope, never()).set("gwen.feature.scenario", "the butterfly flaps its wings")
        verify(mockTopScope, never()).set("gwen.feature.scenario", "The observer")
        verify(mockLifecycle, times(2)).beforeSpec(any[Identifiable], any[Spec], any[ScopedDataStack])
        verify(mockLifecycle, times(2)).afterSpec(any[SpecResult], any[ScopedDataStack])
        verify(mockLifecycle).beforeBackground(any[Identifiable], any[Background], any[ScopedDataStack])
        verify(mockLifecycle).afterBackground(any[Background], any[ScopedDataStack])
        verify(mockLifecycle).beforeStepDef(any[Identifiable], any[Scenario], any[ScopedDataStack])
        verify(mockLifecycle).afterStepDef(any[Scenario], any[ScopedDataStack])
        verify(mockLifecycle, times(2)).beforeScenario(any[Identifiable], any[Scenario], any[ScopedDataStack])
        verify(mockLifecycle, times(2)).afterScenario(any[Scenario], any[ScopedDataStack])
        verify(mockLifecycle, times(12)).beforeStep(any[Identifiable], any[Step], any[ScopedDataStack])
        verify(mockLifecycle, times(12)).afterStep(any[Step], any[ScopedDataStack])
      case None => 
        fail("List(SpecResult) expected")
    }
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
  
  private def writeToFile(content: String, targetFile: File): File = 
    targetFile tap { file =>
      new FileWriter(file) tap { fw =>
        try {
          fw.write(content)
        } finally {
          fw.close()
        }
      }
    }
  
}