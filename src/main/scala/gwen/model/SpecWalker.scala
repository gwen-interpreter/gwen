/*
 * Copyright 2021 Branko Juric, Brady Wood
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

package gwen.model

import gwen.model.gherkin._

/**
  * Walks the nodes of a given specification.
  * 
  * @param spec the specification to walk
  */
abstract class SpecWalker(spec: Specification) {

  // overrideable callbacks
  def onSpecification(parent: Identifiable, spec: Specification): Unit = { }
  def onFeature(parent: Identifiable, feature: Feature): Unit = { }
  def onBackground(parent: Identifiable, background: Background): Unit = { }
  def onRule(parent: Identifiable, rule: Rule): Unit = { }
  def onScenario(parent: Identifiable, scenario: Scenario): Unit = { }
  def onExamples(parent: Identifiable, examples: Examples): Unit = { }
  def onStep(parent: Identifiable, step: Step): Unit = { }

  /**
    * Walks all nodes in a feature specification.
    *
    * @param parent optional parent (default is Root)
    * 
    */
  def walk(parent: Identifiable = Root): Unit = {
    onSpecification(parent, spec)
    onFeature(spec, spec.feature)
    spec.background foreach { background => 
      walk(spec.feature, background)
    }
    spec.scenarios foreach { scenario => 
      walk(spec.feature, scenario)
    }
    spec.rules foreach { rule => 
      walk(spec.feature, rule)
    }
  }
  
  /**
    * Recursively walks all nodes in the tree.  
    *
    * @param parent the parent
    * @param node the node to walk
    */
  private def walk(parent: Identifiable, node: SpecNode): Unit = node match {
    case background: Background =>
      onBackground(parent, background)
      background.steps foreach { step => 
        walk(background, step)
      }
    case rule: Rule =>
      onRule(parent, rule)
      rule.background foreach { background => 
        walk(rule, background)
      }
      rule.scenarios foreach { scenario => 
        walk(rule, scenario)
      }
    case scenario: Scenario =>
      scenario.background foreach { background =>
        walk(parent, background)
      }
      onScenario(parent, scenario)
      scenario.steps foreach { step => 
        walk(scenario, step)
      }
      scenario.examples foreach { exs =>
        walk(scenario, exs)
      }
    case examples: Examples  => 
      onExamples(parent, examples)
      examples.scenarios foreach { scenario => 
        walk(examples, scenario)
      }
    case step: Step =>
      onStep(parent, step)
  }
  
}
