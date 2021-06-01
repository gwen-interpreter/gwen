/*
 * Copyright 2020-2021 Branko Juric, Brady Wood
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

import gwen.core.UUIDGenerator
import gwen.core.node.FeatureUnit
import gwen.core.node.GwenNode
import gwen.core.node.NodeType
import gwen.core.node.gherkin._
import gwen.core.result.SpecResult
import gwen.core.state.ScopedDataStack
import gwen.core.status._

import org.mockito.ArgumentCaptor
import org.mockito.Matchers.any
import org.mockito.Mockito._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatestplus.mockito.MockitoSugar

class NodeEventsTest extends FlatSpec with Matchers with MockitoSugar {

  private def parent(): GwenNode = new GwenNode() { val nodeType: NodeType.Value = NodeType.Root }

  "When pause is not set at disptatcher level then all events" should "be dispatched" in {
      
    val unitEventCaptor = ArgumentCaptor.forClass(classOf[NodeEvent[FeatureUnit]])
    val featureSpecEventCaptor = ArgumentCaptor.forClass(classOf[NodeEvent[Spec]])
    val featureResultEventCaptor = ArgumentCaptor.forClass(classOf[NodeEvent[SpecResult]])
    val scenarioEventCaptor = ArgumentCaptor.forClass(classOf[NodeEvent[Scenario]])
    val backgroundEventCaptor = ArgumentCaptor.forClass(classOf[NodeEvent[Background]])
    val ruleEventCaptor = ArgumentCaptor.forClass(classOf[NodeEvent[Rule]])
    val stepEventCaptor = ArgumentCaptor.forClass(classOf[NodeEvent[Step]])

    val listener = spy(new NodeEventListener("Listener"))
    val dispatcher = new NodeEventDispatcher()

    val unit = mock[FeatureUnit]
    val featureSpec = mock[Spec]
    val featureResult = mock[SpecResult]
    val scenario = mock[Scenario]
    val stepDef = mock[Scenario]
    val background = mock[Background]
    val rule = mock[Rule]
    val step = mock[Step]
    val scopes = mock[ScopedDataStack]

    when(step.evalStatus).thenReturn(Passed(1))

    when(unit.parent).thenReturn(parent())

    dispatcher.addListener(listener)
            
    dispatcher.beforeUnit(unit, scopes)
    verify(listener).beforeUnit(unitEventCaptor.capture())
    unitEventCaptor.getValue().phase should be (NodePhase.before)

    dispatcher.afterUnit(unit, scopes)
    verify(listener).afterUnit(unitEventCaptor.capture())
    unitEventCaptor.getValue().phase should be (NodePhase.after)
    
    dispatcher.beforeSpec(parent(), featureSpec, scopes)
    verify(listener).beforeSpec(featureSpecEventCaptor.capture())
    featureSpecEventCaptor.getValue().phase should be (NodePhase.before)
    
    dispatcher.afterSpec(featureResult, scopes)
    verify(listener).afterSpec(featureResultEventCaptor.capture())
    featureResultEventCaptor.getValue().phase should be (NodePhase.after)
    
    dispatcher.beforeScenario(parent(), scenario, scopes)
    verify(listener).beforeScenario(scenarioEventCaptor.capture())
    scenarioEventCaptor.getValue().phase should be (NodePhase.before)
    
    dispatcher.afterScenario(scenario, scopes)
    verify(listener).afterScenario(scenarioEventCaptor.capture())
    scenarioEventCaptor.getValue().phase should be (NodePhase.after)
    
    dispatcher.beforeBackground(parent(), background, scopes)
    verify(listener).beforeBackground(backgroundEventCaptor.capture())
    backgroundEventCaptor.getValue().phase should be (NodePhase.before)
    
    dispatcher.afterBackground(background, scopes)
    verify(listener).afterBackground(backgroundEventCaptor.capture())
    backgroundEventCaptor.getValue().phase should be (NodePhase.after)
    
    dispatcher.beforeStep(parent(), step, scopes)
    verify(listener).beforeStep(stepEventCaptor.capture())
    stepEventCaptor.getValue().phase should be (NodePhase.before)
    
    dispatcher.afterStep(step, scopes)
    verify(listener).afterStep(stepEventCaptor.capture())
    stepEventCaptor.getValue().phase should be (NodePhase.after)
    
    dispatcher.beforeStepDef(parent(), stepDef, scopes)
    verify(listener).beforeStepDef(scenarioEventCaptor.capture())
    scenarioEventCaptor.getValue().phase should be (NodePhase.before)
    
    dispatcher.afterStepDef(stepDef, scopes)
    verify(listener).afterStepDef(scenarioEventCaptor.capture())
    scenarioEventCaptor.getValue().phase should be (NodePhase.after)
    
    dispatcher.beforeRule(parent(), rule, scopes)
    verify(listener).beforeRule(ruleEventCaptor.capture())
    ruleEventCaptor.getValue().phase should be (NodePhase.before)
    
    dispatcher.afterRule(rule, scopes)
    verify(listener).afterRule(ruleEventCaptor.capture())
    ruleEventCaptor.getValue().phase should be (NodePhase.after)

    dispatcher.healthCheck(parent(), step, scopes)
    verify(listener).healthCheck(stepEventCaptor.capture())
    stepEventCaptor.getValue().phase should be (NodePhase.healthCheck)

  }

  "When one listener bypasses StepDefs then it should not receive those events but other listeners" should "receive all events" in {
      
    val listener1 = spy(new NodeEventListener("Listener1"))
    val listener2 = spy(new NodeEventListener("Listener2", Set(NodeType.StepDef)))
    val listener3 = spy(new NodeEventListener("Listener3"))
    val listeners = List(listener1, listener2, listener3)
    val listeners1and3 = List(listener1, listener3)
    val dispatcher = new NodeEventDispatcher()

    val step1 = mock[Step]
    val step2 = mock[Step]
    val step3 = mock[Step]
    val stepDef1 = mock[Scenario]
    val stepDef2 = mock[Scenario]
    val scopes = mock[ScopedDataStack]
    val stepUuid1 = UUIDGenerator.nextId
    val stepUuid2 = UUIDGenerator.nextId
    val stepUuid3 = UUIDGenerator.nextId
    val stepDefUuid1 = UUIDGenerator.nextId
    val stepDefUuid2 = UUIDGenerator.nextId

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

    dispatcher.addListener(listener1)
    dispatcher.addListener(listener2)
    dispatcher.addListener(listener3)

    dispatcher.beforeStep(parent(), step1, scopes)
    listeners.foreach { listener => 
      verify(listener).beforeStep(any[NodeEvent[Step]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStepDef(parent(),stepDef1, scopes)
    verify(listener2, never()).beforeStepDef(any[NodeEvent[Scenario]])
    listeners1and3.foreach { listener => 
      verify(listener).beforeStepDef(any[NodeEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStep(parent(), step2, scopes)
    verify(listener2, never()).beforeStep(any[NodeEvent[Step]])
    listeners1and3.foreach { listener => 
      verify(listener).beforeStep(any[NodeEvent[Step]])
    }

    dispatcher.afterStep(step2, scopes)
    verify(listener2, never()).afterStep(any[NodeEvent[Step]])
    listeners1and3.foreach { listener => 
      verify(listener).afterStep(any[NodeEvent[Step]])
    }

    dispatcher.beforeStepDef(parent(), stepDef2, scopes)
    verify(listener2, never()).beforeStepDef(any[NodeEvent[Scenario]])
    listeners1and3.foreach { listener => 
      verify(listener).beforeStepDef(any[NodeEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStep(parent(), step3, scopes)
    verify(listener2, never()).beforeStep(any[NodeEvent[Step]])
    listeners1and3.foreach { listener => 
      verify(listener).beforeStep(any[NodeEvent[Step]])
    }

    dispatcher.afterStep(step3, scopes)
    verify(listener2, never()).afterStep(any[NodeEvent[Step]])
    listeners1and3.foreach { listener => 
      verify(listener).afterStep(any[NodeEvent[Step]])
    }

    dispatcher.afterStepDef(stepDef2, scopes)
    verify(listener2, never()).afterStepDef(any[NodeEvent[Scenario]])
    listeners1and3.foreach { listener => 
      verify(listener).afterStepDef(any[NodeEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.afterStepDef(stepDef1, scopes)
    verify(listener2, never()).afterStepDef(any[NodeEvent[Scenario]])
    listeners1and3.foreach { listener => 
      verify(listener).afterStepDef(any[NodeEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.afterStep(step1, scopes)
    listeners.foreach { listener => 
      verify(listener).afterStep(any[NodeEvent[Step]])
    }

  }

  "When one listener bypasses both Meta and StepDefs then it should not receive those events but other listeners" should "receive all events" in {
      
    val listener1 = spy(new NodeEventListener("Listener1"))
    val listener2 = spy(new NodeEventListener("Listener2", Set(NodeType.Meta, NodeType.StepDef)))
    val listener3 = spy(new NodeEventListener("Listener3"))
    val listeners = List(listener1, listener2, listener3)
    val listeners1and3 = List(listener1, listener3)
    val dispatcher = new NodeEventDispatcher()

    val featureSpec = mock[Spec]
    val featureResult = mock[SpecResult]
    val metaSpec = mock[Spec]
    val metaResult = mock[SpecResult]
    val step1 = mock[Step]
    val step2 = mock[Step]
    val step3 = mock[Step]
    val step4 = mock[Step]
    val stepDef1 = mock[Scenario]
    val stepDef2 = mock[Scenario]

    val scopes = mock[ScopedDataStack]

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

    dispatcher.beforeSpec(parent(), metaSpec, scopes)
    verify(listener2, never()).beforeSpec(any[NodeEvent[Spec]])
    listeners1and3.foreach { listener => 
      verify(listener).beforeSpec(any[NodeEvent[Spec]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStep(parent(), step1, scopes)
    verify(listener2, never()).beforeStep(any[NodeEvent[Step]])
    listeners1and3.foreach { listener => 
      verify(listener).beforeStep(any[NodeEvent[Step]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.afterStep(step1, scopes)
    verify(listener2, never()).afterStep(any[NodeEvent[Step]])
    listeners1and3.foreach { listener => 
      verify(listener).afterStep(any[NodeEvent[Step]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStepDef(parent(), stepDef1, scopes)
    verify(listener2, never()).beforeStepDef(any[NodeEvent[Scenario]])
    listeners1and3.foreach { listener => 
      verify(listener).beforeStepDef(any[NodeEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStep(parent(), step2, scopes)
    verify(listener2, never()).beforeStep(any[NodeEvent[Step]])
    listeners1and3.foreach { listener => 
      verify(listener).beforeStep(any[NodeEvent[Step]])
    }

    dispatcher.afterStep(step2, scopes)
    verify(listener2, never()).afterStep(any[NodeEvent[Step]])
    listeners1and3.foreach { listener => 
      verify(listener).afterStep(any[NodeEvent[Step]])
    }

    dispatcher.afterStepDef(stepDef1, scopes)
    verify(listener2, never()).afterStepDef(any[NodeEvent[Scenario]])
    listeners1and3.foreach { listener => 
      verify(listener).afterStepDef(any[NodeEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.afterSpec(metaResult, scopes)
    verify(listener2, never()).afterSpec(any[NodeEvent[SpecResult]])
    listeners1and3.foreach { listener => 
      verify(listener).afterSpec(any[NodeEvent[SpecResult]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeSpec(parent(), featureSpec, scopes)
    listeners.foreach { listener => 
      verify(listener).beforeSpec(any[NodeEvent[Spec]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStepDef(parent(), stepDef2, scopes)
    verify(listener2, never()).beforeStepDef(any[NodeEvent[Scenario]])
    listeners1and3.foreach { listener => 
      verify(listener).beforeStepDef(any[NodeEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStep(parent(), step3, scopes)
    verify(listener2, never()).beforeStep(any[NodeEvent[Step]])
    listeners1and3.foreach { listener => 
      verify(listener).beforeStep(any[NodeEvent[Step]])
    }

    dispatcher.afterStep(step3, scopes)
    verify(listener2, never()).afterStep(any[NodeEvent[Step]])
    listeners1and3.foreach { listener => 
      verify(listener).afterStep(any[NodeEvent[Step]])
    }

    dispatcher.afterStepDef(stepDef2, scopes)
    verify(listener2, never()).afterStepDef(any[NodeEvent[Scenario]])
    listeners1and3.foreach { listener => 
      verify(listener).afterStepDef(any[NodeEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStep(parent(), step4, scopes)
    listeners.foreach { listener => 
      verify(listener).beforeStep(any[NodeEvent[Step]])
    }

    dispatcher.afterStep(step4, scopes)
    listeners.foreach { listener => 
      verify(listener).afterStep(any[NodeEvent[Step]])
    }

    dispatcher.afterSpec(featureResult, scopes)
    listeners.foreach { listener => 
      verify(listener).afterSpec(any[NodeEvent[SpecResult]])
    }
    listeners.foreach(listener => reset(listener))

  }

}