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

import gwen._
import gwen.dsl._

import java.io.File

 /**
  * Enfores certain rules depending on `gwen.feature.mode` and `gwen.behavior.rules` settings.
  */
trait EvalRules {

  /**
    * Checks that a background satisfies behavior rules before evaluation.
    *
    * @param background the background  to check
    * @param specFile the file the background was loaded from
    */
  def checkBackgroundRules(background: Background, specFile: Option[File]): Unit = {
    if (BehaviorRules.isStrict && isFeatureFile(specFile)) {
      if (!isProperBehavior(background.steps)) {
        Errors.improperBehaviorError(background, "Background", specFile)
      }
    }
  }

  /**
    * Checks that a scenario or stepdef satisfies behavior rules before evaluation.
    *
    * @param scenario the scenario to check
    * @param specFile the file the scenario was loaded from
    */
  def checkScenarioRules(scenario: Scenario, specFile: Option[File]): Unit = {
    if (isFeatureFile(specFile)) {
      if (FeatureMode.isDeclarative && scenario.isStepDef) {
        Errors.imperativeStepDefError(scenario, specFile)
      }
      if (BehaviorRules.isStrict && !(FeatureMode.isImperative && scenario.isStepDef)) {
        if (!isProperBehavior(scenario.steps)) {
          Errors.improperBehaviorError(scenario, scenario.keyword, specFile)
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
  def checkStepDefRules[T <: EnvContext](step: Step, env: T): Unit = {
    if (env.isEvaluatingTopLevelStep && env.isEvaluatingFeatureFile) {
      if (BehaviorRules.isStrict) {
        step.stepDef foreach { case (stepDef, _) =>
          stepDef.behaviorTag match {
            case Some(behaviorTag) =>
              checkStepRules(step, BehaviorType.withName(behaviorTag.name), env)
            case _ =>
              Errors.undefinedStepDefBehaviorError(stepDef, stepDef.metaFile)
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
  def checkStepRules[T <: EnvContext](step: Step, actualBehavior: BehaviorType.Value, env: T): Unit = {
    if (env.isEvaluatingTopLevelStep && env.isEvaluatingFeatureFile) {
      if (FeatureMode.isDeclarative && step.stepDef.isEmpty) {
        Errors.imperativeStepError(step)
      }
      if (BehaviorRules.isStrict) {
        env.currentBehavior foreach { expectedBehavior =>
          if (actualBehavior != expectedBehavior) {
            Errors.unexpectedBehaviorError(step, expectedBehavior, actualBehavior, env.specFile)
          } else if (!StepKeyword.isAnd(step.keyword)) {
            val stepBehavior = BehaviorType.of(step.keyword) 
            if (stepBehavior != expectedBehavior) {
              Errors.unexpectedBehaviorError(step, expectedBehavior, stepBehavior, env.specFile)
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

  private def isFeatureFile(specFile: Option[File]):Boolean = 
    specFile.map(FileIO.isFeatureFile).getOrElse(false)

}