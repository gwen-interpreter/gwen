/*
 * Copyright 2020-2024 Branko Juric, Brady Wood
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

package gwen.core.node.event

import gwen.core.BaseTest
import gwen.core.UUIDGenerator
import gwen.core.node.FeatureUnit
import gwen.core.node.NodeType
import gwen.core.node.gherkin._
import gwen.core.result.SpecResult
import gwen.core.state.Environment
import gwen.core.status._

import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalatestplus.mockito.MockitoSugar
import gwen.core.state.EnvState
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.util.Date

class NodeEventListener1 extends NodeEventListener("Listener1") { }
class NodeEventListener2NoStepDef extends NodeEventListener("Listener2", Set(NodeType.StepDef)) { }
class NodeEventListener2NoMetaNoStepDef extends NodeEventListener("Listener2", Set(NodeType.Meta, NodeType.StepDef)) { }
class NodeEventListener3 extends NodeEventListener("Listener3") { }

class NodeEventsTest extends BaseTest with Matchers with MockitoSugar {

  "When pause is not set at disptatcher level then all events" should "be dispatched" in {

    val unitEventCaptor = ArgumentCaptor.forClass(classOf[NodeEvent[FeatureUnit]])
    val featureSpecEventCaptor = ArgumentCaptor.forClass(classOf[NodeEvent[Spec]])
    val featureResultEventCaptor = ArgumentCaptor.forClass(classOf[NodeEvent[SpecResult]])
    val scenarioEventCaptor = ArgumentCaptor.forClass(classOf[NodeEvent[Scenario]])
    val backgroundEventCaptor = ArgumentCaptor.forClass(classOf[NodeEvent[Background]])
    val ruleEventCaptor = ArgumentCaptor.forClass(classOf[NodeEvent[Rule]])
    val stepEventCaptor = ArgumentCaptor.forClass(classOf[NodeEvent[Step]])

    val listener = spy(new NodeEventListener1())
    val dispatcher = new NodeEventDispatcher()

    val unit = mock[FeatureUnit]
    val featureSpec = mock[Spec]
    val feature = mock[Feature]
    val featureResult = mock[SpecResult]
    val scenario = mock[Scenario]
    val stepDef = mock[Scenario]
    val background = mock[Background]
    val rule = mock[Rule]
    val step = mock[Step]

    val env = new Environment(EnvState()) { 
      override def getBoundValue(name: String): String = topScope.get(name)
    }

    when(featureSpec.feature).thenReturn(feature)
    when(featureSpec.specFile).thenReturn(Some(new File("spec.feature")))
    when(featureSpec.specType).thenReturn(SpecType.Feature)

    when(featureResult.spec).thenReturn(featureSpec)
    when(featureResult.finished).thenReturn(new Date())

    when(scenario.evalStatus).thenReturn(Passed(1))
    when(stepDef.evalStatus).thenReturn(Passed(1))
    when(rule.evalStatus).thenReturn(Passed(1))
    when(step.evalStatus).thenReturn(Passed(1))

    dispatcher.addListener(listener)

    dispatcher.beforeUnit(unit, env)
    verify(listener).beforeUnit(unitEventCaptor.capture())
    unitEventCaptor.getValue().phase should be (NodePhase.before)

    dispatcher.afterUnit(unit, env)
    verify(listener).afterUnit(unitEventCaptor.capture())
    unitEventCaptor.getValue().phase should be (NodePhase.after)

    dispatcher.beforeSpec(featureSpec, env)
    verify(listener).beforeSpec(featureSpecEventCaptor.capture())
    featureSpecEventCaptor.getValue().phase should be (NodePhase.before)

    dispatcher.afterSpec(featureResult, env)
    verify(listener).afterSpec(featureResultEventCaptor.capture())
    featureResultEventCaptor.getValue().phase should be (NodePhase.after)

    dispatcher.beforeScenario(scenario, env)
    verify(listener).beforeScenario(scenarioEventCaptor.capture())
    scenarioEventCaptor.getValue().phase should be (NodePhase.before)

    dispatcher.afterScenario(scenario, env)
    verify(listener).afterScenario(scenarioEventCaptor.capture())
    scenarioEventCaptor.getValue().phase should be (NodePhase.after)

    dispatcher.beforeBackground(background, env)
    verify(listener).beforeBackground(backgroundEventCaptor.capture())
    backgroundEventCaptor.getValue().phase should be (NodePhase.before)

    dispatcher.afterBackground(background, env)
    verify(listener).afterBackground(backgroundEventCaptor.capture())
    backgroundEventCaptor.getValue().phase should be (NodePhase.after)

    dispatcher.beforeStep(step, env)
    verify(listener).beforeStep(stepEventCaptor.capture())
    stepEventCaptor.getValue().phase should be (NodePhase.before)

    dispatcher.afterStep(step, env)
    verify(listener).afterStep(stepEventCaptor.capture())
    stepEventCaptor.getValue().phase should be (NodePhase.after)

    dispatcher.beforeStepDef(stepDef, env)
    verify(listener).beforeStepDef(scenarioEventCaptor.capture())
    scenarioEventCaptor.getValue().phase should be (NodePhase.before)

    dispatcher.afterStepDef(stepDef, env)
    verify(listener).afterStepDef(scenarioEventCaptor.capture())
    scenarioEventCaptor.getValue().phase should be (NodePhase.after)

    dispatcher.beforeRule(rule, env)
    verify(listener).beforeRule(ruleEventCaptor.capture())
    ruleEventCaptor.getValue().phase should be (NodePhase.before)

    dispatcher.afterRule(rule, env)
    verify(listener).afterRule(ruleEventCaptor.capture())
    ruleEventCaptor.getValue().phase should be (NodePhase.after)

    dispatcher.healthCheck(step, env)
    verify(listener).healthCheck(stepEventCaptor.capture())
    stepEventCaptor.getValue().phase should be (NodePhase.healthCheck)

  }

  "When one listener bypasses StepDefs then it should not receive those events but other listeners" should "receive all events" in {

    val listener1 = spy(new NodeEventListener1())
    val listener2 = spy(new NodeEventListener2NoStepDef())
    val listener3 = spy(new NodeEventListener3())
    val listeners = List(listener1, listener2, listener3)
    val listeners1and3 = List(listener1, listener3)
    val dispatcher = new NodeEventDispatcher()

    val step1 = mock[Step]
    val step2 = mock[Step]
    val step3 = mock[Step]
    val stepDef1 = mock[Scenario]
    val stepDef2 = mock[Scenario]

    val stepUuid1 = UUIDGenerator.nextId
    val stepUuid2 = UUIDGenerator.nextId
    val stepUuid3 = UUIDGenerator.nextId
    val stepDefUuid1 = UUIDGenerator.nextId
    val stepDefUuid2 = UUIDGenerator.nextId

    val env = new Environment(EnvState()) {
      override def getBoundValue(name: String): String = topScope.get(name)
    }

    when(step1.evalStatus).thenReturn(Passed(1))
    when(step2.evalStatus).thenReturn(Passed(1))
    when(step3.evalStatus).thenReturn(Passed(1))

    when(step1.nodeType).thenReturn(NodeType.Step)
    when(step2.nodeType).thenReturn(NodeType.Step)
    when(step3.nodeType).thenReturn(NodeType.Step)
    when(stepDef1.nodeType).thenReturn(NodeType.StepDef)
    when(stepDef2.nodeType).thenReturn(NodeType.StepDef)
    when(step1.uuid).thenReturn(stepUuid1)
    when(step2.uuid).thenReturn(stepUuid2)
    when(step3.uuid).thenReturn(stepUuid3)
    when(stepDef1.uuid).thenReturn(stepDefUuid1)
    when(stepDef2.uuid).thenReturn(stepDefUuid2)
    when(stepDef1.evalStatus).thenReturn(Passed(1))
    when(stepDef2.evalStatus).thenReturn(Passed(1))

    dispatcher.addListener(listener1)
    dispatcher.addListener(listener2)
    dispatcher.addListener(listener3)

    dispatcher.beforeStep(step1, env)
    listeners.foreach { listener =>
      verify(listener).beforeStep(any[NodeEvent[Step]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStepDef(stepDef1, env)
    verify(listener2, never()).beforeStepDef(any[NodeEvent[Scenario]])
    listeners1and3.foreach { listener =>
      verify(listener).beforeStepDef(any[NodeEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStep(step2, env)
    verify(listener2, never()).beforeStep(any[NodeEvent[Step]])
    listeners1and3.foreach { listener =>
      verify(listener).beforeStep(any[NodeEvent[Step]])
    }

    dispatcher.afterStep(step2, env)
    verify(listener2, never()).afterStep(any[NodeEvent[Step]])
    listeners1and3.foreach { listener =>
      verify(listener).afterStep(any[NodeEvent[Step]])
    }

    dispatcher.beforeStepDef(stepDef2, env)
    verify(listener2, never()).beforeStepDef(any[NodeEvent[Scenario]])
    listeners1and3.foreach { listener =>
      verify(listener).beforeStepDef(any[NodeEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStep(step3, env)
    verify(listener2, never()).beforeStep(any[NodeEvent[Step]])
    listeners1and3.foreach { listener =>
      verify(listener).beforeStep(any[NodeEvent[Step]])
    }

    dispatcher.afterStep(step3, env)
    verify(listener2, never()).afterStep(any[NodeEvent[Step]])
    listeners1and3.foreach { listener =>
      verify(listener).afterStep(any[NodeEvent[Step]])
    }

    dispatcher.afterStepDef(stepDef2, env)
    verify(listener2, never()).afterStepDef(any[NodeEvent[Scenario]])
    listeners1and3.foreach { listener =>
      verify(listener).afterStepDef(any[NodeEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.afterStepDef(stepDef1, env)
    verify(listener2, never()).afterStepDef(any[NodeEvent[Scenario]])
    listeners1and3.foreach { listener =>
      verify(listener).afterStepDef(any[NodeEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.afterStep(step1, env)
    listeners.foreach { listener =>
      verify(listener).afterStep(any[NodeEvent[Step]])
    }

  }

  "When one listener bypasses both Meta and StepDefs then it should not receive those events but other listeners" should "receive all events" in {

    val listener1 = spy(new NodeEventListener1())
    val listener2 = spy(new NodeEventListener2NoMetaNoStepDef())
    val listener3 = spy(new NodeEventListener3())
    val listeners = List(listener1, listener2, listener3)
    val listeners1and3 = List(listener1, listener3)
    val dispatcher = new NodeEventDispatcher()

    val featureSpec = mock[Spec]
    val feature = mock[Feature]
    val featureResult = mock[SpecResult]
    val metaSpec = mock[Spec]
    val meta = mock[Feature]
    val metaResult = mock[SpecResult]
    val step1 = mock[Step]
    val step2 = mock[Step]
    val step3 = mock[Step]
    val step4 = mock[Step]
    val stepDef1 = mock[Scenario]
    val stepDef2 = mock[Scenario]

    val env = new Environment(EnvState()) {
      override def getBoundValue(name: String): String = topScope.get(name)
    }

    val featureSpecUuid = UUIDGenerator.nextId
    val featureResultUuid = UUIDGenerator.nextId
    val metaSpecUuid = UUIDGenerator.nextId
    val metaResultUuid = UUIDGenerator.nextId
    val stepUuid1 = UUIDGenerator.nextId
    val stepUuid2 = UUIDGenerator.nextId
    val stepUuid3 = UUIDGenerator.nextId
    val stepUuid4 = UUIDGenerator.nextId
    val stepDefUuid1 = UUIDGenerator.nextId
    val stepDefUuid2 = UUIDGenerator.nextId

    when(featureSpec.feature).thenReturn(feature)
    when(featureSpec.specFile).thenReturn(Some(new File("spec.feature")))
    when(featureSpec.specType).thenReturn(SpecType.Feature)

    when(featureResult.spec).thenReturn(featureSpec)
    when(featureResult.finished).thenReturn(new Date())

    when(metaSpec.feature).thenReturn(meta)
    when(metaSpec.specFile).thenReturn(Some(new File("spec.meta")))
    when(metaSpec.specType).thenReturn(SpecType.Meta)

    when(metaResult.spec).thenReturn(metaSpec)
    when(metaResult.finished).thenReturn(new Date())

    when(step1.evalStatus).thenReturn(Passed(1))
    when(step2.evalStatus).thenReturn(Passed(1))
    when(step3.evalStatus).thenReturn(Passed(1))
    when(step4.evalStatus).thenReturn(Passed(1))

    when(featureSpec.nodeType).thenReturn(NodeType.Feature)
    when(featureResult.nodeType).thenReturn(NodeType.Result)
    when(metaSpec.nodeType).thenReturn(NodeType.Meta)
    when(metaResult.nodeType).thenReturn(NodeType.Result)
    when(step1.nodeType).thenReturn(NodeType.Step)
    when(step2.nodeType).thenReturn(NodeType.Step)
    when(step3.nodeType).thenReturn(NodeType.Step)
    when(step4.nodeType).thenReturn(NodeType.Step)
    when(stepDef1.nodeType).thenReturn(NodeType.StepDef)
    when(stepDef2.nodeType).thenReturn(NodeType.StepDef)
    when(stepDef1.evalStatus).thenReturn(Passed(1))
    when(stepDef2.evalStatus).thenReturn(Passed(1))

    when(featureSpec.uuid).thenReturn(featureSpecUuid)
    when(featureResult.uuid).thenReturn(featureResultUuid)
    when(metaSpec.uuid).thenReturn(metaSpecUuid)
    when(metaResult.uuid).thenReturn(metaResultUuid)
    when(step1.uuid).thenReturn(stepUuid1)
    when(step2.uuid).thenReturn(stepUuid2)
    when(step3.uuid).thenReturn(stepUuid3)
    when(step4.uuid).thenReturn(stepUuid4)
    when(stepDef1.uuid).thenReturn(stepDefUuid1)
    when(stepDef2.uuid).thenReturn(stepDefUuid2)

    dispatcher.addListener(listener1)
    dispatcher.addListener(listener2)
    dispatcher.addListener(listener3)

    dispatcher.beforeSpec(metaSpec, env)
    verify(listener2, never()).beforeSpec(any[NodeEvent[Spec]])
    listeners1and3.foreach { listener =>
      verify(listener).beforeSpec(any[NodeEvent[Spec]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStep(step1, env)
    verify(listener2, never()).beforeStep(any[NodeEvent[Step]])
    listeners1and3.foreach { listener =>
      verify(listener).beforeStep(any[NodeEvent[Step]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.afterStep(step1, env)
    verify(listener2, never()).afterStep(any[NodeEvent[Step]])
    listeners1and3.foreach { listener =>
      verify(listener).afterStep(any[NodeEvent[Step]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStepDef(stepDef1, env)
    verify(listener2, never()).beforeStepDef(any[NodeEvent[Scenario]])
    listeners1and3.foreach { listener =>
      verify(listener).beforeStepDef(any[NodeEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStep(step2, env)
    verify(listener2, never()).beforeStep(any[NodeEvent[Step]])
    listeners1and3.foreach { listener =>
      verify(listener).beforeStep(any[NodeEvent[Step]])
    }

    dispatcher.afterStep(step2, env)
    verify(listener2, never()).afterStep(any[NodeEvent[Step]])
    listeners1and3.foreach { listener =>
      verify(listener).afterStep(any[NodeEvent[Step]])
    }

    dispatcher.afterStepDef(stepDef1, env)
    verify(listener2, never()).afterStepDef(any[NodeEvent[Scenario]])
    listeners1and3.foreach { listener =>
      verify(listener).afterStepDef(any[NodeEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.afterSpec(metaResult, env)
    verify(listener2, never()).afterSpec(any[NodeEvent[SpecResult]])
    listeners1and3.foreach { listener =>
      verify(listener).afterSpec(any[NodeEvent[SpecResult]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeSpec(featureSpec, env)
    listeners.foreach { listener =>
      verify(listener).beforeSpec(any[NodeEvent[Spec]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStepDef(stepDef2, env)
    verify(listener2, never()).beforeStepDef(any[NodeEvent[Scenario]])
    listeners1and3.foreach { listener =>
      verify(listener).beforeStepDef(any[NodeEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStep(step3, env)
    verify(listener2, never()).beforeStep(any[NodeEvent[Step]])
    listeners1and3.foreach { listener =>
      verify(listener).beforeStep(any[NodeEvent[Step]])
    }

    dispatcher.afterStep(step3, env)
    verify(listener2, never()).afterStep(any[NodeEvent[Step]])
    listeners1and3.foreach { listener =>
      verify(listener).afterStep(any[NodeEvent[Step]])
    }

    dispatcher.afterStepDef(stepDef2, env)
    verify(listener2, never()).afterStepDef(any[NodeEvent[Scenario]])
    listeners1and3.foreach { listener =>
      verify(listener).afterStepDef(any[NodeEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStep(step4, env)
    listeners.foreach { listener =>
      verify(listener).beforeStep(any[NodeEvent[Step]])
    }

    dispatcher.afterStep(step4, env)
    listeners.foreach { listener =>
      verify(listener).afterStep(any[NodeEvent[Step]])
    }

    dispatcher.afterSpec(featureResult, env)
    listeners.foreach { listener =>
      verify(listener).afterSpec(any[NodeEvent[SpecResult]])
    }
    listeners.foreach(listener => reset(listener))

  }

}
