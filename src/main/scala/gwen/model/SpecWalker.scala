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
  * Walks the nodes of a given specification and accumuates a value of type T.
  * 
  * @param spec the specification to walk
  */
abstract class SpecWalker[T](spec: Specification) {

  // overrideable callbacks
  def onSpecification(parent: Identifiable, spec: Specification, acc: T): T = acc
  def onFeature(parent: Identifiable, feature: Feature, acc: T): T = acc
  def onBackground(parent: Identifiable, background: Background, acc: T): T = acc
  def onRule(parent: Identifiable, rule: Rule, acc: T): T = acc
  def onScenario(parent: Identifiable, scenario: Scenario, acc: T): T = acc
  def onExamples(parent: Identifiable, examples: Examples, acc: T): T = acc
  def onStep(parent: Identifiable, step: Step, acc: T): T = acc

  /**
    * Walks all nodes in a feature specification and accumulates a value.
    *
    * @param parent the parent node (or Root)
    * @param zero the zero (start) accumulator value
    */
  def walk(parent: Identifiable, zero: T): T = {
    var acc = onSpecification(parent, spec, zero)
    acc = onFeature(spec, spec.feature, acc)
    spec.background foreach { background => 
      acc = walk(spec.feature, background, acc)
    }
    spec.scenarios foreach { scenario => 
      acc = walk(spec.feature, scenario, acc)
    }
    spec.rules foreach { rule => 
      acc = walk(spec.feature, rule, acc)
    }
    acc
  }
  
  /**
    * Recursively walks all nodes in the tree.  
    *
    * @param parent the parent
    * @param node the node to walk
    * @param current the current accumulator value
    */
  private def walk(parent: Identifiable, node: SpecNode, current: T): T = {
    var acc = current
    node match {
      case background: Background =>
        acc = onBackground(parent, background, acc)
        background.steps foreach { step => 
          acc = walk(background, step, acc)
        }
        acc
      case rule: Rule =>
        acc = onRule(parent, rule, acc)
        rule.background foreach { background => 
          acc = walk(rule, background, acc)
        }
        rule.scenarios foreach { scenario => 
          acc = walk(rule, scenario, acc)
        }
        acc
      case scenario: Scenario =>
        scenario.background foreach { background =>
          acc = walk(parent, background, acc)
        }
        acc = onScenario(parent, scenario, acc)
        scenario.steps foreach { step => 
          acc = walk(scenario, step, acc)
        }
        scenario.examples foreach { exs =>
          acc = walk(scenario, exs, acc)
        }
        acc
      case examples: Examples  => 
        acc = onExamples(parent, examples, acc)
        examples.scenarios foreach { scenario => 
          acc = walk(examples, scenario, acc)
        }
        acc
      case step: Step =>
        onStep(parent, step, acc)
    }
  }
  
}
