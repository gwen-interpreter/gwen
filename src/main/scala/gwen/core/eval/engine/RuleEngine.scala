/*
 * Copyright 2021-2024 Branko Juric, Brady Wood
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
import gwen.core.data.DataRecord
import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Rule
import gwen.core.node.gherkin.Spec
import gwen.core.status._

import scala.util.chaining._

import com.typesafe.scalalogging.LazyLogging

/**
  * Rule evaluation engine.
  */
trait RuleEngine[T <: EvalContext] extends LazyLogging with ImplicitValueKeys {
  engine: EvalEngine[T] =>

  private [engine] def evaluateRules(spec: Spec, rules: List[Rule], dataRecord: Option[DataRecord], ctx: T): List[Rule] = {
    rules.foldLeft(List[Rule]()) {
      (acc: List[Rule], rule: Rule) =>
        evaluateOrTransitionRule(spec, rule, dataRecord, ctx, acc) :: acc
    } reverse
  }

  private def evaluateOrTransitionRule(parent: GwenNode, rule: Rule, dataRecord: Option[DataRecord], ctx: T, acc: List[Rule]): Rule = {
    EvalStatus(acc.map(_.evalStatus)) match {
      case status @ Failed(_, error) =>
        val isSoftAssert = ctx.evaluate(false) { status.isSoftAssertionError }
        val failfast = ctx.evaluate(false) { GwenSettings.`gwen.feature.failfast.enabled` }
        val exitOnFail = ctx.evaluate(false) { GwenSettings.`gwen.feature.failfast.exit` }
        if (failfast && !exitOnFail && !isSoftAssert) {
          transitionRule(rule, Skipped, ctx)
        } else if (exitOnFail && !isSoftAssert) {
          transitionRule(rule, rule.evalStatus, ctx)
        } else {
          ctx.ruleScope.boundary(rule.name, Nil) {
            beforeRule(rule, ctx)
            logger.info(s"Evaluating ${rule.keyword}: $rule")
            rule.copy(
              withScenarios = evaluateScenarios(rule, rule.scenarios, dataRecord, ctx)
            ) tap { r =>
              afterRule(r, ctx)
              logStatus(ctx.options, r)
            }
          }
        }
      case _ =>
        ctx.ruleScope.boundary(rule.name, Nil) {
          beforeRule(rule, ctx)
          logger.info(s"Evaluating ${rule.keyword}: $rule")
          rule.copy(
            withScenarios = evaluateScenarios(rule, rule.scenarios, dataRecord, ctx)
          ) tap { r =>
            afterRule(r, ctx)
            logStatus(ctx.options, r)
          }
        }
    }
  }

}
