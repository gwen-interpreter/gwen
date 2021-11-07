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

package gwen.core.node.gherkin

import gwen.core.node.GwenNode

/**
  * Walks the nodes of a specification and accumuates a result of type T.
  */
abstract class SpecWalker[T](deep: Boolean) {

  // overridable callbacks
  def onSpec(parent: GwenNode, spec: Spec, acc: T): T = acc
  def onFeature(parent: GwenNode, feature: Feature, acc: T): T = acc
  def onBackground(parent: GwenNode, background: Background, acc: T): T = acc
  def onRule(parent: GwenNode, rule: Rule, acc: T): T = acc
  def onScenario(parent: GwenNode, scenario: Scenario, acc: T): T = acc
  def onExamples(parent: GwenNode, examples: Examples, acc: T): T = acc
  def onStep(parent: GwenNode, step: Step, acc: T): T = acc
  
  /**
    * Recursively walks all nodes in the tree.  
    *
    * @param parent the parent
    * @param node the node to walk
    * @param zero the initial accumulator value
    */
  def walk(parent: GwenNode, node: GwenNode, zero: T): T = {
    var acc = zero
    node match {
      case spec: Spec =>
        acc = onSpec(parent, spec, acc)
        if (deep) {
          acc = walk(spec, spec.feature, acc)
          spec.background foreach { background => 
            acc = walk(spec.feature, background, acc)
          }
          spec.scenarios foreach { scenario => 
            acc = walk(spec.feature, scenario, acc)
          }
          spec.rules foreach { rule => 
            acc = walk(spec.feature, rule, acc)
          }
        }
        acc
      case feature: Feature =>
        acc = onFeature(parent, feature, acc)
        acc
      case background: Background =>
        acc = onBackground(parent, background, acc)
        if (deep) {
          background.steps foreach { step => 
            acc = walk(background, step, acc)
          }
        }
        acc
      case rule: Rule =>
        acc = onRule(parent, rule, acc)
        if (deep) {
          rule.background foreach { background => 
            acc = walk(rule, background, acc)
          }
          rule.scenarios foreach { scenario => 
            acc = walk(rule, scenario, acc)
          }
        }
        acc
      case scenario: Scenario =>
        if (deep) {
          scenario.background foreach { background =>
            acc = walk(scenario, background, acc)
          }
        }
        if (!scenario.isStepDef) {
          acc = onScenario(parent, scenario, acc)
          if (deep) {
            scenario.steps foreach { step => 
              acc = walk(scenario, step, acc)
            }
            scenario.examples foreach { exs =>
              acc = walk(scenario, exs, acc)
            }
          }
        }
        acc
      case examples: Examples  => 
        acc = onExamples(parent, examples, acc)
        if (deep) {
          examples.scenarios foreach { scenario => 
            acc = walk(examples, scenario, acc)
          }
        }
        acc
      case step: Step =>
        onStep(parent, step, acc)
    }
  }
  
}
