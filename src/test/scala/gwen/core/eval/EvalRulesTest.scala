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

package gwen.core.eval

import gwen.core.BaseTest
import gwen.core.Errors._
import gwen.core.TestModel
import gwen.core.model._
import gwen.core.model.gherkin._

import org.scalatest.Matchers

import java.io.File

class EvalRulesTest extends BaseTest with Matchers with GherkinParser with EvalRules with TestModel {

    private def createScenario(steps: String): Scenario = {
        val input = s"""
        | Scenario: scenario
        |     $steps
        | """.stripMargin
        parseSpec(s"Feature: feature\n$input").map(_.scenarios.head).get
    }

    private def createBackground(steps: String): Background = {
        val input = s"""
        | Background: background
        |       $steps
        | """.stripMargin
        parseSpec(s"Feature: feature\n$input").map(_.background.head).get
    }

    private def createStep(step: String): Step = parseStep(step).get

    val env = new EvalEnvironment() {
        topScope.pushObject("spec.file", featureFile)
    }

    val featureFile = new File(getClass.getResource("/gwen/evalrules/InlinedStepDef.feature").getFile)
    val feature = parseSpec(featureFile).get
    val givenStep = createStep("Given I set the context")
    val whenStep = createStep("When I peform an action")
    val thenStep = createStep("Then this should happen")
    val andStep = createStep("And this")
    val butStep = createStep("But that")

    "declartive feature with stepdef" should "fail declarative rules check" in {    
        withSetting("gwen.feature.mode", "declarative") {
            val stepDef = feature.scenarios(1)
            intercept[ImperativeStepDefException] {
                checkScenarioRules(stepDef, SpecType.Feature)
            }
            checkScenarioRules(stepDef, SpecType.Meta)
        }
    }

    "imperative feature with stepdef" should "pass imperative rules check" in {
        withSetting("gwen.feature.mode", "imperative") {
            SpecType.values.foreach { specType =>
                val stepDef = feature.scenarios(1)
                checkScenarioRules(stepDef, specType)
            }
        }
    }

    "imperative feature with stepdef with behavior tag" should "pass strict behavior rules check" in {
        withSetting("gwen.feature.mode", "imperative") {
            withSetting("gwen.behavior.rules", "strict") {
                SpecType.values.foreach { specType =>
                    val stepDef = feature.scenarios(1)
                    checkScenarioRules(stepDef, specType)
                }
            }
        }
    }

    "imperative feature with stepdef but no behavior tag" should "fail strict behavior rules check" in {
        withSetting("gwen.feature.mode", "imperative") {
            withSetting("gwen.behavior.rules", "strict") {
                val stepDef = feature.scenarios(1)  
                val stepDefNobehaviorTag = Scenario(
                    List(Tag("@StepDef")),
                    stepDef.name,
                    stepDef.description,
                    stepDef.background,
                    stepDef.steps,
                    stepDef.examples)
                val step = Step(feature.scenarios(0).steps(2), stepDefNobehaviorTag, stepDefNobehaviorTag.steps.flatMap(_.attachments))
                withSpecType(SpecType.Feature) { _ =>
                    intercept[UndefinedStepDefBehaviorException] {
                        checkStepDefRules(step, env)
                    }
                }
                withSpecType(SpecType.Meta) { _ =>
                    checkStepDefRules(step, env)
                }
            }
        }
    }

    "declartive feature with imperative step" should "fail declarative rules check" in {
        withSetting("gwen.feature.mode", "declarative") {
            val step = feature.scenarios(0).steps(1)
            withSpecType(SpecType.Feature) { _ =>
                intercept[ImperativeStepException] {
                    checkStepRules(step, BehaviorType.Action, env)
                }
            }
            withSpecType(SpecType.Meta) { _ =>
                checkStepRules(step, BehaviorType.Action, env)
            }
        }
    }

    "imperative feature with imperative step" should "pass imperative rules check" in {
        withSetting("gwen.feature.mode", "imperative") {
            val step = feature.scenarios(0).steps(1)
            SpecType.values foreach { specType =>
                withSpecType(specType) { _ =>
                    checkStepRules(step, BehaviorType.Action, env)
                }
            }
        }
    }

    "scenarios or backgrounds with G-W-T sequence" should "pass strict and lenient behavior rules checks" in {
        
        val steps = """
        |     Given I set the context
        |      When I perform an action
        |      Then this should happen
        | """.stripMargin
        val scenario = createScenario(steps)
        val background = createBackground(steps)

        SpecType.values foreach { specType =>
            withSetting("gwen.behavior.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behavior.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }
    }

    "scenarios or backgrounds with G-W-T-B sequence" should "pass strict and lenient behavior rules checks" in {
        
        val steps = """
        |     Given I set the context
        |      When I perform an action
        |      Then this should happen
        |       But this should not happen
        | """.stripMargin
        val scenario = createScenario(steps)
        val background = createBackground(steps)

        SpecType.values foreach { specType =>
            withSetting("gwen.behavior.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behavior.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }
    }

    "scenarios or backgrounds with G-A-W-A-T-A-B sequence" should "pass strict and lenient behavior rules checks" in {
        
        val steps = """
        |     Given I set some context
        |       And I set more context
        |      When I perform an action
        |       And I perform another action
        |      Then this should happen
        |       And that should happen
        |       But this should not happen
        | """.stripMargin
        val scenario = createScenario(steps)
        val background = createBackground(steps)

        SpecType.values foreach { specType =>
            withSetting("gwen.behavior.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behavior.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }
    }

    "scenarios or backgrounds with G-W-B-T sequence" should "pass imperative and fail declartive rules checks" in {
        
        val steps = """
        |     Given I set some context
        |      When I perform an action
        |       But this should not happen
        |      Then this should happen
        | """.stripMargin
        val scenario = createScenario(steps)
        val background = createBackground(steps)

        withSpecType(SpecType.Feature) { specType =>
            withSetting("gwen.behavior.rules", "strict") {
                intercept[ImproperBehaviorException] {
                    checkScenarioRules(scenario, specType)
                }
                intercept[ImproperBehaviorException] {
                    checkBackgroundRules(background, specType)
                }
            }
            withSetting("gwen.behavior.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }

        withSpecType(SpecType.Meta) { specType =>
            withSetting("gwen.behavior.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behavior.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }
    }

    "scenarios or backgrounds with G-W-T-B-B sequence" should "pass imperative and fail declartive rules checks" in {
        
        val steps = """
        |     Given I set some context
        |      When I perform an action
        |      Then this should happen
        |       But this should not happen
        |       But this should not happen too
        | """.stripMargin
        val scenario = createScenario(steps)
        val background = createBackground(steps)

        withSpecType(SpecType.Feature) { specType =>
            withSetting("gwen.behavior.rules", "strict") {
                intercept[ImproperBehaviorException] {
                    checkScenarioRules(scenario, specType)
                }
                intercept[ImproperBehaviorException] {
                    checkBackgroundRules(background, specType)
                }
            }
            withSetting("gwen.behavior.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }

        withSpecType(SpecType.Meta) { specType =>
            withSetting("gwen.behavior.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behavior.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }
    }

    "scenarios or backgrounds with G-W sequence" should "pass imperative and fail declartive rules checks" in {
        
        val steps = """
        |     Given I set some context
        |      When I perform an action
        | """.stripMargin
        val scenario = createScenario(steps)
        val background = createBackground(steps)

        withSpecType(SpecType.Feature) { specType =>
            withSetting("gwen.behavior.rules", "strict") {
                intercept[ImproperBehaviorException] {
                    checkScenarioRules(scenario, specType)
                }
                intercept[ImproperBehaviorException] {
                    checkBackgroundRules(background, specType)
                }
            }
            withSetting("gwen.behavior.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }

        withSpecType(SpecType.Meta) { specType =>
            withSetting("gwen.behavior.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behavior.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }
    }

    "scenarios or backgrounds with G-W-T-G sequence" should "pass imperative and fail declartive rules checks" in {
        
        val steps = """
        |     Given I set some context
        |      When I perform an action
        |      Then this should happen
        |     Given I set another context 
        | """.stripMargin
        val scenario = createScenario(steps)
        val background = createBackground(steps)

        withSpecType(SpecType.Feature) { specType =>
            withSetting("gwen.behavior.rules", "strict") {
                intercept[ImproperBehaviorException] {
                    checkScenarioRules(scenario, specType)
                }
                intercept[ImproperBehaviorException] {
                    checkBackgroundRules(background, specType)
                }
            }
            withSetting("gwen.behavior.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }

        withSpecType(SpecType.Meta) { specType =>
            withSetting("gwen.behavior.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behavior.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }
    }

    "scenarios or backgrounds with G-W-T-W-T sequence" should "pass imperative and fail declartive rules checks" in {
        
        val steps = """
        |     Given I set some context
        |      When I perform an action
        |      Then this should happen
        |      When I perform another action
        |      Then that should happen
        | """.stripMargin
        val scenario = createScenario(steps)
        val background = createBackground(steps)

        withSpecType(SpecType.Feature) { specType =>
            withSetting("gwen.behavior.rules", "strict") {
                intercept[ImproperBehaviorException] {
                    checkScenarioRules(scenario, specType)
                }
                intercept[ImproperBehaviorException] {
                    checkBackgroundRules(background, specType)
                }
            }
            withSetting("gwen.behavior.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }

        withSpecType(SpecType.Meta) { specType =>
            withSetting("gwen.behavior.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behavior.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }
    }

    "scenarios or backgrounds with G-W-B sequence" should "pass imperative and fail declartive rules checks" in {
        
        val steps = """
        |     Given I set some context
        |      When I perform an action
        |       But this should not happen
        | """.stripMargin
        val scenario = createScenario(steps)
        val background = createBackground(steps)

        withSpecType(SpecType.Feature) { specType =>
            withSetting("gwen.behavior.rules", "strict") {
                intercept[ImproperBehaviorException] {
                    checkScenarioRules(scenario, specType)
                }
                intercept[ImproperBehaviorException] {
                    checkBackgroundRules(background, specType)
                }
            }
            withSetting("gwen.behavior.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }

        withSpecType(SpecType.Meta) { specType =>
            withSetting("gwen.behavior.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behavior.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }
    }

    "scenarios or backgrounds with W-T sequence" should "pass imperative and fail declartive rules checks" in {
        
        val steps = """
        |      When I perform an action
        |      Then this should happen
        | """.stripMargin
        val scenario = createScenario(steps)
        val background = createBackground(steps)

        withSpecType(SpecType.Feature) { specType =>
            withSetting("gwen.behavior.rules", "strict") {
                intercept[ImproperBehaviorException] {
                    checkScenarioRules(scenario, specType)
                }
                intercept[ImproperBehaviorException] {
                    checkBackgroundRules(background, specType)
                }
            }
            withSetting("gwen.behavior.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }

        withSpecType(SpecType.Meta) { specType =>
            withSetting("gwen.behavior.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behavior.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }
    }

    "scenarios or backgrounds with G sequence" should "pass imperative and fail declartive rules checks" in {
        
        val steps = """
        |     Given I set some context
        | """.stripMargin
        val scenario = createScenario(steps)
        val background = createBackground(steps)

        withSpecType(SpecType.Feature) { specType =>
            withSetting("gwen.behavior.rules", "strict") {
                intercept[ImproperBehaviorException] {
                    checkScenarioRules(scenario, specType)
                }
                intercept[ImproperBehaviorException] {
                    checkBackgroundRules(background, specType)
                }
            }
            withSetting("gwen.behavior.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }

        withSpecType(SpecType.Meta) { specType =>
            withSetting("gwen.behavior.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behavior.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }
    }

    "Cntext steps" should "pass strict Context rules checks" in {
        val behavior = BehaviorType.Context
        env.addBehavior(behavior)
        withSpecType(SpecType.Feature) { _ =>
            withSetting("gwen.behavior.rules", "strict") {
                checkStepRules(givenStep, behavior, env)
                checkStepRules(andStep, behavior, env)
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(whenStep, behavior, env)
                }
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(thenStep, behavior, env)
                }
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(butStep, behavior, env)
                }
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(givenStep, BehaviorType.Action, env)
                }
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(andStep, BehaviorType.Action, env)
                }
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(givenStep, BehaviorType.Assertion, env)
                }
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(andStep, BehaviorType.Assertion, env)
                }
            }
        }

        withSpecType(SpecType.Meta) { _ =>
            withSetting("gwen.behavior.rules", "strict") {
                checkStepRules(givenStep, behavior, env)
                checkStepRules(andStep, behavior, env)
                checkStepRules(whenStep, behavior, env)
                checkStepRules(thenStep, behavior, env)
                checkStepRules(butStep, behavior, env)
                checkStepRules(givenStep, BehaviorType.Action, env)
                checkStepRules(andStep, BehaviorType.Action, env)
                checkStepRules(givenStep, BehaviorType.Assertion, env)
                checkStepRules(andStep, BehaviorType.Assertion, env)
            }
        }
    }

    "Action steps" should "pass strict Action rules checks" in {
        val behavior = BehaviorType.Action
        env.addBehavior(behavior)
        withSpecType(SpecType.Feature) { _ =>
            withSetting("gwen.behavior.rules", "strict") {
                checkStepRules(whenStep, behavior, env)
                checkStepRules(andStep, behavior, env)
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(givenStep, behavior, env)
                }
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(thenStep, behavior, env)
                }
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(butStep, behavior, env)
                }
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(whenStep, BehaviorType.Context, env)
                }
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(andStep, BehaviorType.Context, env)
                }
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(whenStep, BehaviorType.Assertion, env)
                }
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(andStep, BehaviorType.Assertion, env)
                }
            }
        }

        withSpecType(SpecType.Meta) { _ =>
            withSetting("gwen.behavior.rules", "strict") {
                checkStepRules(whenStep, behavior, env)
                checkStepRules(andStep, behavior, env)
                checkStepRules(givenStep, behavior, env)
                checkStepRules(thenStep, behavior, env)
                checkStepRules(butStep, behavior, env)
                checkStepRules(whenStep, BehaviorType.Context, env)
                checkStepRules(andStep, BehaviorType.Context, env)
                checkStepRules(whenStep, BehaviorType.Assertion, env)
                checkStepRules(andStep, BehaviorType.Assertion, env)
            }
        }
    }

    "Assertion steps" should "pass strict Assertion rules checks" in {
        val behavior = BehaviorType.Assertion
        env.addBehavior(behavior)
        withSpecType(SpecType.Feature) { _ =>
            withSetting("gwen.behavior.rules", "strict") {
                checkStepRules(thenStep, behavior, env)
                checkStepRules(andStep, behavior, env)
                checkStepRules(butStep, behavior, env)
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(givenStep, behavior, env)
                }
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(whenStep, behavior, env)
                }
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(thenStep, BehaviorType.Context, env)
                }
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(andStep, BehaviorType.Context, env)
                }
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(butStep, BehaviorType.Context, env)
                }
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(thenStep, BehaviorType.Action, env)
                }
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(andStep, BehaviorType.Action, env)
                }
                intercept[UnexpectedBehaviorException] {
                    checkStepRules(butStep, BehaviorType.Action, env)
                }
            }
        }

        withSpecType(SpecType.Meta) { _ =>
            withSetting("gwen.behavior.rules", "strict") {
                checkStepRules(thenStep, behavior, env)
                checkStepRules(andStep, behavior, env)
                checkStepRules(butStep, behavior, env)
                checkStepRules(givenStep, behavior, env)
                checkStepRules(whenStep, behavior, env)
                checkStepRules(thenStep, BehaviorType.Context, env)
                checkStepRules(andStep, BehaviorType.Context, env)
                checkStepRules(butStep, BehaviorType.Context, env)
                checkStepRules(thenStep, BehaviorType.Action, env)
                checkStepRules(andStep, BehaviorType.Action, env)
                checkStepRules(butStep, BehaviorType.Action, env)
            }
        }
    }

    "Steps of any behavior" should "pass lenient behavior rules checks" in {
        SpecType.values.foreach { specType =>
            withSpecType(specType) { _ =>
                withSetting("gwen.behavior.rules", "lenient") {
                    BehaviorType.values foreach { behavior =>
                        env.addBehavior(behavior)
                        checkStepRules(givenStep, behavior, env)
                        checkStepRules(whenStep, behavior, env)
                        checkStepRules(thenStep, behavior, env)
                        checkStepRules(andStep, behavior, env)
                        checkStepRules(butStep, behavior, env)
                    }
                }
            }
        }
    }

    private def withSpecType[T](specType: SpecType.Value)(body: SpecType.Value => T): Unit = {
        val key = SpecType.toString
        env.topScope.pushObject(key, specType)
        try {
            body(specType)
        } finally {
            env.topScope.popObject(key)
        }
    }
    
}