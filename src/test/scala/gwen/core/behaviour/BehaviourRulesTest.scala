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

import gwen.core.BaseTest
import gwen.core.Errors._
import gwen.core.TestModel
import gwen.core.node.gherkin._
import gwen.core.state.Environment
import gwen.core.state.EnvState


import java.io.File
import org.scalatest.matchers.should.Matchers

class BehaviourRulesTest extends BaseTest with Matchers with GherkinParser with BehaviourRules with TestModel {

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

    val env = new Environment(EnvState()) {
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

    "imperative feature with stepdef with behaviour tag" should "pass strict behaviour rules check" in {
        withSetting("gwen.feature.mode", "imperative") {
            withSetting("gwen.behaviour.rules", "strict") {
                SpecType.values.foreach { specType =>
                    val stepDef = feature.scenarios(1)
                    checkScenarioRules(stepDef, specType)
                }
            }
        }
    }

    "imperative feature with stepdef but no behaviour tag" should "fail strict behaviour rules check" in {
        withSetting("gwen.feature.mode", "imperative") {
            withSetting("gwen.behaviour.rules", "strict") {
                val stepDef = feature.scenarios(1)  
                val stepDefNobehaviourTag = Scenario(
                    List(Tag("@StepDef")),
                    stepDef.name,
                    stepDef.description,
                    stepDef.background,
                    stepDef.steps,
                    stepDef.examples)
                val step = Step(feature.scenarios(0).steps(2), stepDefNobehaviourTag, stepDefNobehaviourTag.steps.flatMap(_.attachments))
                withSpecType(SpecType.Feature) { _ =>
                    intercept[UndefinedStepDefBehaviourException] {
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
                    checkStepRules(step, BehaviourType.Action, env)
                }
            }
            withSpecType(SpecType.Meta) { _ =>
                checkStepRules(step, BehaviourType.Action, env)
            }
        }
    }

    "imperative feature with imperative step" should "pass imperative rules check" in {
        withSetting("gwen.feature.mode", "imperative") {
            val step = feature.scenarios(0).steps(1)
            SpecType.values foreach { specType =>
                withSpecType(specType) { _ =>
                    checkStepRules(step, BehaviourType.Action, env)
                }
            }
        }
    }

    "scenarios or backgrounds with G-W-T sequence" should "pass strict and lenient behaviour rules checks" in {
        
        val steps = """
        |     Given I set the context
        |      When I perform an action
        |      Then this should happen
        | """.stripMargin
        val scenario = createScenario(steps)
        val background = createBackground(steps)

        SpecType.values foreach { specType =>
            withSetting("gwen.behaviour.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behaviour.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }
    }

    "scenarios or backgrounds with G-W-T-B sequence" should "pass strict and lenient behaviour rules checks" in {
        
        val steps = """
        |     Given I set the context
        |      When I perform an action
        |      Then this should happen
        |       But this should not happen
        | """.stripMargin
        val scenario = createScenario(steps)
        val background = createBackground(steps)

        SpecType.values foreach { specType =>
            withSetting("gwen.behaviour.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behaviour.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }
    }

    "scenarios or backgrounds with G-A-W-A-T-A-B sequence" should "pass strict and lenient behaviour rules checks" in {
        
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
            withSetting("gwen.behaviour.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behaviour.rules", "lenient") {
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
            withSetting("gwen.behaviour.rules", "strict") {
                intercept[ImproperBehaviourException] {
                    checkScenarioRules(scenario, specType)
                }
                intercept[ImproperBehaviourException] {
                    checkBackgroundRules(background, specType)
                }
            }
            withSetting("gwen.behaviour.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }

        withSpecType(SpecType.Meta) { specType =>
            withSetting("gwen.behaviour.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behaviour.rules", "lenient") {
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
            withSetting("gwen.behaviour.rules", "strict") {
                intercept[ImproperBehaviourException] {
                    checkScenarioRules(scenario, specType)
                }
                intercept[ImproperBehaviourException] {
                    checkBackgroundRules(background, specType)
                }
            }
            withSetting("gwen.behaviour.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }

        withSpecType(SpecType.Meta) { specType =>
            withSetting("gwen.behaviour.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behaviour.rules", "lenient") {
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
            withSetting("gwen.behaviour.rules", "strict") {
                intercept[ImproperBehaviourException] {
                    checkScenarioRules(scenario, specType)
                }
                intercept[ImproperBehaviourException] {
                    checkBackgroundRules(background, specType)
                }
            }
            withSetting("gwen.behaviour.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }

        withSpecType(SpecType.Meta) { specType =>
            withSetting("gwen.behaviour.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behaviour.rules", "lenient") {
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
            withSetting("gwen.behaviour.rules", "strict") {
                intercept[ImproperBehaviourException] {
                    checkScenarioRules(scenario, specType)
                }
                intercept[ImproperBehaviourException] {
                    checkBackgroundRules(background, specType)
                }
            }
            withSetting("gwen.behaviour.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }

        withSpecType(SpecType.Meta) { specType =>
            withSetting("gwen.behaviour.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behaviour.rules", "lenient") {
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
            withSetting("gwen.behaviour.rules", "strict") {
                intercept[ImproperBehaviourException] {
                    checkScenarioRules(scenario, specType)
                }
                intercept[ImproperBehaviourException] {
                    checkBackgroundRules(background, specType)
                }
            }
            withSetting("gwen.behaviour.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }

        withSpecType(SpecType.Meta) { specType =>
            withSetting("gwen.behaviour.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behaviour.rules", "lenient") {
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
            withSetting("gwen.behaviour.rules", "strict") {
                intercept[ImproperBehaviourException] {
                    checkScenarioRules(scenario, specType)
                }
                intercept[ImproperBehaviourException] {
                    checkBackgroundRules(background, specType)
                }
            }
            withSetting("gwen.behaviour.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }

        withSpecType(SpecType.Meta) { specType =>
            withSetting("gwen.behaviour.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behaviour.rules", "lenient") {
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
            withSetting("gwen.behaviour.rules", "strict") {
                intercept[ImproperBehaviourException] {
                    checkScenarioRules(scenario, specType)
                }
                intercept[ImproperBehaviourException] {
                    checkBackgroundRules(background, specType)
                }
            }
            withSetting("gwen.behaviour.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }

        withSpecType(SpecType.Meta) { specType =>
            withSetting("gwen.behaviour.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behaviour.rules", "lenient") {
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
            withSetting("gwen.behaviour.rules", "strict") {
                intercept[ImproperBehaviourException] {
                    checkScenarioRules(scenario, specType)
                }
                intercept[ImproperBehaviourException] {
                    checkBackgroundRules(background, specType)
                }
            }
            withSetting("gwen.behaviour.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }

        withSpecType(SpecType.Meta) { specType =>
            withSetting("gwen.behaviour.rules", "strict") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
            withSetting("gwen.behaviour.rules", "lenient") {
                checkScenarioRules(scenario, specType)
                checkBackgroundRules(background, specType)
            }
        }
    }

    "Cntext steps" should "pass strict Context rules checks" in {
        val behaviour = BehaviourType.Context
        env.addBehaviour(behaviour)
        withSpecType(SpecType.Feature) { _ =>
            withSetting("gwen.behaviour.rules", "strict") {
                checkStepRules(givenStep, behaviour, env)
                checkStepRules(andStep, behaviour, env)
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(whenStep, behaviour, env)
                }
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(thenStep, behaviour, env)
                }
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(butStep, behaviour, env)
                }
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(givenStep, BehaviourType.Action, env)
                }
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(andStep, BehaviourType.Action, env)
                }
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(givenStep, BehaviourType.Assertion, env)
                }
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(andStep, BehaviourType.Assertion, env)
                }
            }
        }

        withSpecType(SpecType.Meta) { _ =>
            withSetting("gwen.behaviour.rules", "strict") {
                checkStepRules(givenStep, behaviour, env)
                checkStepRules(andStep, behaviour, env)
                checkStepRules(whenStep, behaviour, env)
                checkStepRules(thenStep, behaviour, env)
                checkStepRules(butStep, behaviour, env)
                checkStepRules(givenStep, BehaviourType.Action, env)
                checkStepRules(andStep, BehaviourType.Action, env)
                checkStepRules(givenStep, BehaviourType.Assertion, env)
                checkStepRules(andStep, BehaviourType.Assertion, env)
            }
        }
    }

    "Action steps" should "pass strict Action rules checks" in {
        val behaviour = BehaviourType.Action
        env.addBehaviour(behaviour)
        withSpecType(SpecType.Feature) { _ =>
            withSetting("gwen.behaviour.rules", "strict") {
                checkStepRules(whenStep, behaviour, env)
                checkStepRules(andStep, behaviour, env)
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(givenStep, behaviour, env)
                }
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(thenStep, behaviour, env)
                }
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(butStep, behaviour, env)
                }
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(whenStep, BehaviourType.Context, env)
                }
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(andStep, BehaviourType.Context, env)
                }
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(whenStep, BehaviourType.Assertion, env)
                }
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(andStep, BehaviourType.Assertion, env)
                }
            }
        }

        withSpecType(SpecType.Meta) { _ =>
            withSetting("gwen.behaviour.rules", "strict") {
                checkStepRules(whenStep, behaviour, env)
                checkStepRules(andStep, behaviour, env)
                checkStepRules(givenStep, behaviour, env)
                checkStepRules(thenStep, behaviour, env)
                checkStepRules(butStep, behaviour, env)
                checkStepRules(whenStep, BehaviourType.Context, env)
                checkStepRules(andStep, BehaviourType.Context, env)
                checkStepRules(whenStep, BehaviourType.Assertion, env)
                checkStepRules(andStep, BehaviourType.Assertion, env)
            }
        }
    }

    "Assertion steps" should "pass strict Assertion rules checks" in {
        val behaviour = BehaviourType.Assertion
        env.addBehaviour(behaviour)
        withSpecType(SpecType.Feature) { _ =>
            withSetting("gwen.behaviour.rules", "strict") {
                checkStepRules(thenStep, behaviour, env)
                checkStepRules(andStep, behaviour, env)
                checkStepRules(butStep, behaviour, env)
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(givenStep, behaviour, env)
                }
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(whenStep, behaviour, env)
                }
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(thenStep, BehaviourType.Context, env)
                }
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(andStep, BehaviourType.Context, env)
                }
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(butStep, BehaviourType.Context, env)
                }
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(thenStep, BehaviourType.Action, env)
                }
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(andStep, BehaviourType.Action, env)
                }
                intercept[UnexpectedBehaviourException] {
                    checkStepRules(butStep, BehaviourType.Action, env)
                }
            }
        }

        withSpecType(SpecType.Meta) { _ =>
            withSetting("gwen.behaviour.rules", "strict") {
                checkStepRules(thenStep, behaviour, env)
                checkStepRules(andStep, behaviour, env)
                checkStepRules(butStep, behaviour, env)
                checkStepRules(givenStep, behaviour, env)
                checkStepRules(whenStep, behaviour, env)
                checkStepRules(thenStep, BehaviourType.Context, env)
                checkStepRules(andStep, BehaviourType.Context, env)
                checkStepRules(butStep, BehaviourType.Context, env)
                checkStepRules(thenStep, BehaviourType.Action, env)
                checkStepRules(andStep, BehaviourType.Action, env)
                checkStepRules(butStep, BehaviourType.Action, env)
            }
        }
    }

    "Steps of any behaviour" should "pass lenient behaviour rules checks" in {
        SpecType.values.foreach { specType =>
            withSpecType(specType) { _ =>
                withSetting("gwen.behaviour.rules", "lenient") {
                    BehaviourType.values foreach { behaviour =>
                        env.addBehaviour(behaviour)
                        checkStepRules(givenStep, behaviour, env)
                        checkStepRules(whenStep, behaviour, env)
                        checkStepRules(thenStep, behaviour, env)
                        checkStepRules(andStep, behaviour, env)
                        checkStepRules(butStep, behaviour, env)
                    }
                }
            }
        }
    }

    private def withSpecType[T](specType: SpecType)(body: SpecType => T): Unit = {
        val key = SpecType.toString
        env.topScope.pushObject(key, specType)
        try {
            body(specType)
        } finally {
            env.topScope.popObject(key)
        }
    }
    
}