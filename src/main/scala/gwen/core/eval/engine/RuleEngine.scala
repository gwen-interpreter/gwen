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

package gwen.core.eval.engine

import gwen.core._
import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Rule
import gwen.core.node.gherkin.Spec
import gwen.core.status._

import com.typesafe.scalalogging.LazyLogging

/**
  * Rule evaluation engine.
  */
trait RuleEngine[T <: EvalContext] extends LazyLogging {
  engine: EvalEngine[T] =>
  
  private [engine] def evaluateRules(spec: Spec, rules: List[Rule], ctx: T): List[Rule] = {
    rules.foldLeft(List[Rule]()) {
      (acc: List[Rule], rule: Rule) =>
        evaluateOrTransitionRule(spec, rule, ctx, acc) :: acc
    } reverse
  }

  private def evaluateOrTransitionRule(parent: GwenNode, rule: Rule, ctx: T, acc: List[Rule]): Rule = {
    ctx.topScope.set("gwen.rule.name", rule.name)
    EvalStatus(acc.map(_.evalStatus)) match {
      case status @ Failed(_, error) =>
        val isAssertionError = status.isAssertionError
        val isSoftAssert = ctx.evaluate(false) { isAssertionError && AssertionMode.isSoft }
        val failfast = ctx.evaluate(false) { GwenSettings.`gwen.feature.failfast` }
        val exitOnFail = ctx.evaluate(false) { GwenSettings.`gwen.feature.failfast.exit` }
        if (failfast && !exitOnFail && !isSoftAssert) {
          transitionRule(rule, Skipped, ctx)
        } else if (exitOnFail && !isSoftAssert) {
          transitionRule(rule, rule.evalStatus, ctx)
        } else {
          beforeRule(rule, ctx)
          logger.info(s"Evaluating ${rule.keyword}: $rule")
          rule.copy(
            withScenarios = evaluateScenarios(rule, rule.scenarios, ctx)
          ) tap { r =>
            logStatus(r)
            afterRule(r, ctx)
          }
        }
      case _ =>
        beforeRule(rule, ctx)
        logger.info(s"Evaluating ${rule.keyword}: $rule")
        rule.copy(
          withScenarios = evaluateScenarios(rule, rule.scenarios, ctx)
        ) tap { r =>
          logStatus(r)
          afterRule(r, ctx)
        }
    }
  }

}
