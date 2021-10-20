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

package gwen.core.behaviour

import gwen.core._
import gwen.core.node.gherkin._
import gwen.core.state.Environment

 /**
  * Enfores certain rules depending on `gwen.feature.mode` and `gwen.behaviour.rules` settings.
  */
trait BehaviourRules {

  /**
    * Checks that a background satisfies behaviour rules before evaluation.
    *
    * @param background the background  to check
    * @param specType the spec type currently being evaluated
    */
  def checkBackgroundRules(background: Background, specType: SpecType): Unit = {
    if (BehaviourMode.isStrict && specType.isFeature) {
      if (!isProperBehaviour(background.steps)) {
        Errors.improperBehaviourError(background)
      }
    }
  }

  /**
    * Checks that a scenario or stepdef satisfies behaviour rules before evaluation.
    *
    * @param scenario the scenario to check
    * @param specType the spec type currently being evaluated
    */
  def checkScenarioRules(scenario: Scenario, specType: SpecType): Unit = {
    if (specType.isFeature) {
      if (FeatureMode.isDeclarative && scenario.isStepDef) {
        Errors.imperativeStepDefError(scenario)
      }
      if (BehaviourMode.isStrict && !(FeatureMode.isImperative && scenario.isStepDef)) {
        if (!isProperBehaviour(scenario.steps)) {
          Errors.improperBehaviourError(scenario)
        }
      }
    }
  }

  /**
    * Checks that a step def called in a feature satisfies behavoiur rules at evaluation.
    *
    * @param step the step to check
    * @param env the environment context
    */
  def checkStepDefRules(step: Step, env: Environment): Unit = {
    if (env.specType.isFeature && env.isEvaluatingTopLevelStep) {
      if (BehaviourMode.isStrict) {
        step.stepDef foreach { stepDef =>
          if (!stepDef.isSynthetic) {
            stepDef.behaviourTag match {
              case Some(behaviourTag) =>
                checkStepRules(step, BehaviourType.valueOf(behaviourTag.name), env)
              case _ =>
                Errors.undefinedStepDefBehaviourError(stepDef)
            }
          }
        }
      }
    }
  }

  /**
    * Checks that a step called in a feature satisfies behavoiur rules at evaluation.
    *
    * @param step the step to check
    * @param actualBehaviour the actual behaviour type of the step
    * @param env the environment context
    */
  def checkStepRules(step: Step, actualBehaviour: BehaviourType, env: Environment): Unit = {
    if (env.specType.isFeature && env.isEvaluatingTopLevelStep) {
      if (FeatureMode.isDeclarative && step.stepDef.isEmpty) {
        Errors.imperativeStepError(step)
      }
      if (BehaviourMode.isStrict) {
        env.currentBehaviour foreach { expectedBehaviour =>
          if (actualBehaviour != expectedBehaviour) {
            Errors.unexpectedBehaviourError(step, expectedBehaviour, actualBehaviour)
          } else if (!StepKeyword.isAnd(step.keyword)) {
            val stepBehaviour = BehaviourType.of(step.keyword) 
            if (stepBehaviour != expectedBehaviour) {
              Errors.unexpectedBehaviourError(step, expectedBehaviour, stepBehaviour)
            }
          }
        }
      }
    }
  }

  private def isProperBehaviour(steps: List[Step]): Boolean = {
    val order = steps.map(_.keyword).filter(k => !StepKeyword.isAnd(k)).map { keyword => 
      StepKeyword.valueOf(keyword).toString
    }
    order.mkString("-").matches("Given-When-Then(-But)?")
  }

}