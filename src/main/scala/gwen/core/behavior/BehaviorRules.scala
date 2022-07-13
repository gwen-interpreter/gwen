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

package gwen.core.behavior

import gwen.core._
import gwen.core.node.gherkin._
import gwen.core.state.Environment

 /**
  * Enfores certain rules depending on `gwen.feature.mode` and `gwen.behavior.rules` settings.
  */
trait BehaviorRules {

  /**
    * Checks that a background satisfies behavior rules before evaluation.
    *
    * @param background the background  to check
    * @param specType the spec type currently being evaluated
    */
  def checkBackgroundRules(background: Background, specType: SpecType): Unit = {
    if (BehaviorMode.isStrict && specType.isFeature) {
      if (!isProperBehavior(background.steps)) {
        Errors.improperBehaviorError(background)
      }
    }
  }

  /**
    * Checks that a scenario or stepdef satisfies behavior rules before evaluation.
    *
    * @param scenario the scenario to check
    * @param specType the spec type currently being evaluated
    */
  def checkScenarioRules(scenario: Scenario, specType: SpecType): Unit = {
    if (specType.isFeature) {
      if (FeatureMode.isDeclarative && scenario.isStepDef) {
        Errors.imperativeStepDefError(scenario)
      }
      if (BehaviorMode.isStrict && !(FeatureMode.isImperative && scenario.isStepDef)) {
        if (!isProperBehavior(scenario.steps)) {
          Errors.improperBehaviorError(scenario)
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
      if (BehaviorMode.isStrict) {
        step.stepDef foreach { stepDef =>
          if (!stepDef.isSynthetic) {
            stepDef.behaviorTag match {
              case Some(behaviorTag) =>
                checkStepRules(step, BehaviorType.valueOf(behaviorTag.name), env)
              case _ =>
                Errors.undefinedStepDefBehaviorError(stepDef)
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
    * @param actualBehavior the actual behavior type of the step
    * @param env the environment context
    */
  def checkStepRules(step: Step, actualBehavior: BehaviorType, env: Environment): Unit = {
    if (env.specType.isFeature && env.isEvaluatingTopLevelStep && !(step.isData || step.isNoData)) {
      if (FeatureMode.isDeclarative && step.stepDef.isEmpty) {
        Errors.imperativeStepError(step)
      }
      if (BehaviorMode.isStrict) {
        env.currentBehavior foreach { expectedBehavior =>
          if (actualBehavior != expectedBehavior) {
            Errors.unexpectedBehaviorError(step, expectedBehavior, actualBehavior)
          } else if (!StepKeyword.isAnd(step.keyword)) {
            val stepBehavior = BehaviorType.of(step.keyword) 
            if (stepBehavior != expectedBehavior) {
              Errors.unexpectedBehaviorError(step, expectedBehavior, stepBehavior)
            }
          }
        }
      }
    }
  }

  private def isProperBehavior(steps: List[Step]): Boolean = {
    val order = steps.map(_.keyword).filter(k => !StepKeyword.isAnd(k)).map { keyword => 
      StepKeyword.valueOf(keyword).toString
    }
    order.mkString("-").matches("Given-When-Then(-But)?")
  }

}