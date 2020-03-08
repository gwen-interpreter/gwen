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
import gwen.errors._
import gwen.GwenSettings
import gwen.Predefs.FileIO
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
        improperBehaviorError(background, "Background", specFile)
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
        imperativeStepDefError(scenario, specFile)
      }
      if (BehaviorRules.isStrict && !(FeatureMode.isImperative && scenario.isStepDef)) {
        if (!isProperBehavior(scenario.steps)) {
          improperBehaviorError(scenario, scenario.keyword, specFile)
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
        step.stepDef foreach { stepDef =>
          stepDef.behaviorTag match {
            case Some(behaviorTag) =>
              checkStepRules(step, BehaviorType.withName(behaviorTag.name), env)
            case _ =>
              undefinedStepDefBehaviorError(stepDef, stepDef.metaFile)
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
        imperativeStepError(step)
      }
      if (BehaviorRules.isStrict) {
        env.currentBehavior foreach { expectedBehavior =>
          if (actualBehavior != expectedBehavior) {
            unexpectedBehaviorError(step, expectedBehavior, actualBehavior, env.specFile)
          } else if (step.keyword != StepKeyword.And) {
            val stepBehavior = BehaviorType.of(step.keyword) 
            if (stepBehavior != expectedBehavior) {
              unexpectedBehaviorError(step, expectedBehavior, stepBehavior, env.specFile)
            }
          }
        }
      }
    }
  }

  private def isProperBehavior(steps: List[Step]): Boolean =
    steps.map(_.keyword).filter(_ != StepKeyword.And).mkString("-").matches("Given-When-Then(-But)?")

  private def isFeatureFile(specFile: Option[File]):Boolean = 
    specFile.map(FileIO.isFeatureFile).getOrElse(false)

}