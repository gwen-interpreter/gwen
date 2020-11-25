/*
 * Copyright 2020 Branko Juric, Brady Wood
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

import gwen.dsl._
import gwen.UUIDGenerator

import org.mockito.ArgumentCaptor
import org.mockito.Matchers.any
import org.mockito.Mockito._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatestplus.mockito.MockitoSugar

class LifecycleEventsTest extends FlatSpec with Matchers with MockitoSugar {

  private def parent(): Identifiable = new Identifiable() { val nodeType: NodeType.Value = NodeType.Root }

  "When pause is not set at disptatcher level then all events" should "be dispatched" in {
      
    val unitEventCaptor = ArgumentCaptor.forClass(classOf[LifecycleEvent[FeatureUnit]])
    val featureSpecEventCaptor = ArgumentCaptor.forClass(classOf[LifecycleEvent[FeatureSpec]])
    val featureResultEventCaptor = ArgumentCaptor.forClass(classOf[LifecycleEvent[FeatureResult]])
    val scenarioEventCaptor = ArgumentCaptor.forClass(classOf[LifecycleEvent[Scenario]])
    val backgroundEventCaptor = ArgumentCaptor.forClass(classOf[LifecycleEvent[Background]])
    val ruleEventCaptor = ArgumentCaptor.forClass(classOf[LifecycleEvent[Rule]])
    val stepEventCaptor = ArgumentCaptor.forClass(classOf[LifecycleEvent[Step]])

    val listener = spy(new LifecycleEventListener("Listener"))
    val dispatcher = new LifecycleEventDispatcher()

    val unit = mock[FeatureUnit]
    val featureSpec = mock[FeatureSpec]
    val featureResult = mock[FeatureResult]
    val scenario = mock[Scenario]
    val stepDef = mock[Scenario]
    val background = mock[Background]
    val rule = mock[Rule]
    val step = mock[Step]

    dispatcher.addListener(listener)
            
    dispatcher.beforeUnit(unit)
    verify(listener).beforeUnit(unitEventCaptor.capture())
    unitEventCaptor.getValue().phase should be (LifecyclePhase.before)

    dispatcher.afterUnit(unit)
    verify(listener).afterUnit(unitEventCaptor.capture())
    unitEventCaptor.getValue().phase should be (LifecyclePhase.after)
    
    dispatcher.beforeFeature(parent(), featureSpec)
    verify(listener).beforeFeature(featureSpecEventCaptor.capture())
    featureSpecEventCaptor.getValue().phase should be (LifecyclePhase.before)
    
    dispatcher.afterFeature(featureResult)
    verify(listener).afterFeature(featureResultEventCaptor.capture())
    featureResultEventCaptor.getValue().phase should be (LifecyclePhase.after)
    
    dispatcher.beforeScenario(parent(), scenario)
    verify(listener).beforeScenario(scenarioEventCaptor.capture())
    scenarioEventCaptor.getValue().phase should be (LifecyclePhase.before)
    
    dispatcher.afterScenario(scenario)
    verify(listener).afterScenario(scenarioEventCaptor.capture())
    scenarioEventCaptor.getValue().phase should be (LifecyclePhase.after)
    
    dispatcher.beforeBackground(parent(), background)
    verify(listener).beforeBackground(backgroundEventCaptor.capture())
    backgroundEventCaptor.getValue().phase should be (LifecyclePhase.before)
    
    dispatcher.afterBackground(background)
    verify(listener).afterBackground(backgroundEventCaptor.capture())
    backgroundEventCaptor.getValue().phase should be (LifecyclePhase.after)
    
    dispatcher.beforeStep(parent(), step)
    verify(listener).beforeStep(stepEventCaptor.capture())
    stepEventCaptor.getValue().phase should be (LifecyclePhase.before)
    
    dispatcher.afterStep(step)
    verify(listener).afterStep(stepEventCaptor.capture())
    stepEventCaptor.getValue().phase should be (LifecyclePhase.after)
    
    dispatcher.beforeStepDef(parent(), stepDef)
    verify(listener).beforeStepDef(scenarioEventCaptor.capture())
    scenarioEventCaptor.getValue().phase should be (LifecyclePhase.before)
    
    dispatcher.afterStepDef(stepDef)
    verify(listener).afterStepDef(scenarioEventCaptor.capture())
    scenarioEventCaptor.getValue().phase should be (LifecyclePhase.after)
    
    dispatcher.beforeRule(parent(), rule)
    verify(listener).beforeRule(ruleEventCaptor.capture())
    ruleEventCaptor.getValue().phase should be (LifecyclePhase.before)
    
    dispatcher.afterRule(rule)
    verify(listener).afterRule(ruleEventCaptor.capture())
    ruleEventCaptor.getValue().phase should be (LifecyclePhase.after)

  }

  "When one listener bypasses StepDefs then it should not receive those events but other listeners" should "receive all events" in {
      
    val listener1 = spy(new LifecycleEventListener("Listener1"))
    val listener2 = spy(new LifecycleEventListener("Listener2", Set(NodeType.StepDef)))
    val listener3 = spy(new LifecycleEventListener("Listener3"))
    val listeners = List(listener1, listener2, listener3)
    val listeners1and3 = List(listener1, listener3)
    val dispatcher = new LifecycleEventDispatcher()

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

    dispatcher.beforeStep(parent(), step1)
    listeners.foreach { listener => 
      verify(listener).beforeStep(any[LifecycleEvent[Step]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStepDef(parent(),stepDef1)
    verify(listener2, never()).beforeStepDef(any[LifecycleEvent[Scenario]])
    listeners1and3.foreach { listener => 
      verify(listener).beforeStepDef(any[LifecycleEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStep(parent(), step2)
    verify(listener2, never()).beforeStep(any[LifecycleEvent[Step]])
    listeners1and3.foreach { listener => 
      verify(listener).beforeStep(any[LifecycleEvent[Step]])
    }

    dispatcher.afterStep(step2)
    verify(listener2, never()).afterStep(any[LifecycleEvent[Step]])
    listeners1and3.foreach { listener => 
      verify(listener).afterStep(any[LifecycleEvent[Step]])
    }

    dispatcher.beforeStepDef(parent(), stepDef2)
    verify(listener2, never()).beforeStepDef(any[LifecycleEvent[Scenario]])
    listeners1and3.foreach { listener => 
      verify(listener).beforeStepDef(any[LifecycleEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStep(parent(), step3)
    verify(listener2, never()).beforeStep(any[LifecycleEvent[Step]])
    listeners1and3.foreach { listener => 
      verify(listener).beforeStep(any[LifecycleEvent[Step]])
    }

    dispatcher.afterStep(step3)
    verify(listener2, never()).afterStep(any[LifecycleEvent[Step]])
    listeners1and3.foreach { listener => 
      verify(listener).afterStep(any[LifecycleEvent[Step]])
    }

    dispatcher.afterStepDef(stepDef2)
    verify(listener2, never()).afterStepDef(any[LifecycleEvent[Scenario]])
    listeners1and3.foreach { listener => 
      verify(listener).afterStepDef(any[LifecycleEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.afterStepDef(stepDef1)
    verify(listener2, never()).afterStepDef(any[LifecycleEvent[Scenario]])
    listeners1and3.foreach { listener => 
      verify(listener).afterStepDef(any[LifecycleEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.afterStep(step1)
    listeners.foreach { listener => 
      verify(listener).afterStep(any[LifecycleEvent[Step]])
    }

  }

  "When one listener bypasses both Meta and StepDefs then it should not receive those events but other listeners" should "receive all events" in {
      
    val listener1 = spy(new LifecycleEventListener("Listener1"))
    val listener2 = spy(new LifecycleEventListener("Listener2", Set(NodeType.Meta, NodeType.StepDef)))
    val listener3 = spy(new LifecycleEventListener("Listener3"))
    val listeners = List(listener1, listener2, listener3)
    val listeners1and3 = List(listener1, listener3)
    val dispatcher = new LifecycleEventDispatcher()

    val featureSpec = mock[FeatureSpec]
    val featureResult = mock[FeatureResult]
    val metaSpec = mock[FeatureSpec]
    val metaResult = mock[FeatureResult]
    val step1 = mock[Step]
    val step2 = mock[Step]
    val step3 = mock[Step]
    val step4 = mock[Step]
    val stepDef1 = mock[Scenario]
    val stepDef2 = mock[Scenario]

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

    dispatcher.beforeFeature(parent(), metaSpec)
    verify(listener2, never()).beforeFeature(any[LifecycleEvent[FeatureSpec]])
    listeners1and3.foreach { listener => 
      verify(listener).beforeFeature(any[LifecycleEvent[FeatureSpec]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStep(parent(), step1)
    verify(listener2, never()).beforeStep(any[LifecycleEvent[Step]])
    listeners1and3.foreach { listener => 
      verify(listener).beforeStep(any[LifecycleEvent[Step]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.afterStep(step1)
    verify(listener2, never()).afterStep(any[LifecycleEvent[Step]])
    listeners1and3.foreach { listener => 
      verify(listener).afterStep(any[LifecycleEvent[Step]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStepDef(parent(), stepDef1)
    verify(listener2, never()).beforeStepDef(any[LifecycleEvent[Scenario]])
    listeners1and3.foreach { listener => 
      verify(listener).beforeStepDef(any[LifecycleEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStep(parent(), step2)
    verify(listener2, never()).beforeStep(any[LifecycleEvent[Step]])
    listeners1and3.foreach { listener => 
      verify(listener).beforeStep(any[LifecycleEvent[Step]])
    }

    dispatcher.afterStep(step2)
    verify(listener2, never()).afterStep(any[LifecycleEvent[Step]])
    listeners1and3.foreach { listener => 
      verify(listener).afterStep(any[LifecycleEvent[Step]])
    }

    dispatcher.afterStepDef(stepDef1)
    verify(listener2, never()).afterStepDef(any[LifecycleEvent[Scenario]])
    listeners1and3.foreach { listener => 
      verify(listener).afterStepDef(any[LifecycleEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.afterFeature(metaResult)
    verify(listener2, never()).afterFeature(any[LifecycleEvent[FeatureResult]])
    listeners1and3.foreach { listener => 
      verify(listener).afterFeature(any[LifecycleEvent[FeatureResult]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeFeature(parent(), featureSpec)
    listeners.foreach { listener => 
      verify(listener).beforeFeature(any[LifecycleEvent[FeatureSpec]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStepDef(parent(), stepDef2)
    verify(listener2, never()).beforeStepDef(any[LifecycleEvent[Scenario]])
    listeners1and3.foreach { listener => 
      verify(listener).beforeStepDef(any[LifecycleEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStep(parent(), step3)
    verify(listener2, never()).beforeStep(any[LifecycleEvent[Step]])
    listeners1and3.foreach { listener => 
      verify(listener).beforeStep(any[LifecycleEvent[Step]])
    }

    dispatcher.afterStep(step3)
    verify(listener2, never()).afterStep(any[LifecycleEvent[Step]])
    listeners1and3.foreach { listener => 
      verify(listener).afterStep(any[LifecycleEvent[Step]])
    }

    dispatcher.afterStepDef(stepDef2)
    verify(listener2, never()).afterStepDef(any[LifecycleEvent[Scenario]])
    listeners1and3.foreach { listener => 
      verify(listener).afterStepDef(any[LifecycleEvent[Scenario]])
    }
    listeners.foreach(listener => reset(listener))

    dispatcher.beforeStep(parent(), step4)
    listeners.foreach { listener => 
      verify(listener).beforeStep(any[LifecycleEvent[Step]])
    }

    dispatcher.afterStep(step4)
    listeners.foreach { listener => 
      verify(listener).afterStep(any[LifecycleEvent[Step]])
    }

    dispatcher.afterFeature(featureResult)
    listeners.foreach { listener => 
      verify(listener).afterFeature(any[LifecycleEvent[FeatureResult]])
    }
    listeners.foreach(listener => reset(listener))

  }

}