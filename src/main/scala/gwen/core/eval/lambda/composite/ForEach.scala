/*
 * Copyright 2021-2023 Branko Juric, Brady Wood
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

package gwen.core.eval.lambda.composite

import gwen.core._
import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.eval.binding.DryValueBinding
import gwen.core.eval.lambda.CompositeStep
import gwen.core.node.GwenNode
import gwen.core.node.gherkin._
import gwen.core.node.gherkin.table.DataTable
import gwen.core.node.gherkin.table.TableOrientation
import gwen.core.state.ScopedData
import gwen.core.status._

import scala.util.Try

abstract class ForEach[T <: EvalContext](engine: EvalEngine[T], doStep: String) extends CompositeStep[T](doStep) with ImplicitValueKeys {

  /**
    * Repeats a step for each item in list of items of type U.
    */
  def evaluateForEach[U](itemList: ()=>Seq[U], name: String, parent: GwenNode, step: Step, ctx: T): Step = {
    val keyword = FeatureKeyword.nameOf(FeatureKeyword.Scenario)
    val items = itemList()
    if (items.nonEmpty) {
      val foreachSteps = items.toList.zipWithIndex map { case (_, index) => 
        step.copy(
          withKeyword = if (index == 0) step.keyword else StepKeyword.nameOf(StepKeyword.And)
        )
      }
      val tableTypeTag: Option[Tag] = ctx.topScope.getObject(DataTable.tableKey) match {
        case Some(table: DataTable) =>
          table.orientation match {
            case TableOrientation.horizontal => Some(Tag(Annotations.HorizontalTable))
            case TableOrientation.vertical => Some(Tag(Annotations.VerticalTable))
          }
        case _ => None
      }
      val tags = List(Tag(Annotations.Synthetic), Tag(Annotations.ForEach), Tag(Annotations.StepDef)) ++ tableTypeTag.toList
      val preForeachStepDef = Scenario(None, tags, keyword, name, None, Nil, None, foreachSteps, Nil, Nil, step.cumulativeParams)
      engine.beforeStepDef(preForeachStepDef, ctx)
      val noOfElements = items.size
      val steps =
        items match {
          case Nil =>
            logger.info(s"For-each[$name]: none found")
            Nil
          case _ =>
            logger.info(s"For-each[$name]: $noOfElements found")
            val prevNameValue = ctx.topScope.getOpt(name)
            try {
              items.zipWithIndex.foldLeft(List[Step]()) { case (acc, (currentElement, index)) =>
                val occurence = Occurrence(index + 1, noOfElements)
                val params: List[(String, String)] = currentElement match {
                  case data: ScopedData => 
                    if (data.scope == DataTable.recordKey) {
                      ctx.topScope.pushObject(DataTable.recordKey, data)
                    } else {
                      ctx.topScope.pushObject(name, data)
                    }
                    data.findEntries { _ => true } toList
                  case value: String => 
                    ctx.topScope.set(name, value, force = true)
                    if (ctx.options.dryRun) {
                      ctx.topScope.pushObject(name, currentElement)
                    }
                    List((name, value))
                  case _ =>
                    ctx.topScope.pushObject(name, currentElement)
                    List((name, s"$name ${occurence.number}"))
                }
                val prevIndex = ctx.topScope.featureScope.getOpt(`gwen.iteration.index`)
                val prevNumber = ctx.topScope.featureScope.getOpt(`gwen.iteration.number`)
                val descriptor = s"[$name] $occurence"
                ctx.topScope.featureScope.set(`gwen.iteration.index`, occurence.index.toString)
                ctx.topScope.featureScope.set(`gwen.iteration.number`, occurence.number.toString)
                (try {
                  EvalStatus(acc.map(_.evalStatus)) match {
                    case status @ Failed(_, error)  =>
                      val isSoftAssert = ctx.evaluate(false) { status.isSoftAssertionError }
                      val failfast = ctx.evaluate(false) { GwenSettings.`gwen.feature.failfast.enabled` }
                      if (failfast && !isSoftAssert) {
                        logger.info(s"Skipping $descriptor")
                        engine.transitionStep(foreachSteps(index).copy(withParams = params), Skipped, ctx)
                      } else {
                        logger.info(s"Processing $descriptor")
                        engine.evaluateStep(preForeachStepDef, Step(step.sourceRef, if (index == 0) step.keyword else StepKeyword.nameOf(StepKeyword.And), doStep, Nil, None, Nil, None, Pending, params, Nil, Nil, None, Nil), ctx)
                      }
                    case _ =>
                      logger.info(s"Processing $descriptor")
                      engine.evaluateStep(preForeachStepDef, Step(step.sourceRef, if (index == 0) step.keyword else StepKeyword.nameOf(StepKeyword.And), doStep, Nil, None, Nil, None, Pending, params, Nil, Nil, None, Nil), ctx)
                  }
                } finally {
                  currentElement match {
                    case data: ScopedData if data.scope == DataTable.recordKey => ctx.topScope.popObject(DataTable.recordKey)
                    case _ => ctx.topScope.popObject(name)
                  }
                  ctx.topScope.featureScope.set(`gwen.iteration.number`, prevNumber.orNull)
                  ctx.topScope.featureScope.set(`gwen.iteration.index`, prevIndex.orNull)
                }) :: acc
              } reverse
            } finally {
              ctx.topScope.set(name, prevNameValue.orNull, force = true)
            }
        }
      val foreachStepDef = preForeachStepDef.copy(withSteps = steps)
      engine.afterStepDef(foreachStepDef, ctx)
      step.copy(withStepDef = Some(foreachStepDef))
    } else {
      engine.transitionStep(step, Passed(0, abstained = !ctx.options.dryRun), ctx)
    }
  }

}
