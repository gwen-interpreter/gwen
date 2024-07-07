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
import gwen.core.node._
import gwen.core.node.gherkin.Dialect
import gwen.core.node.gherkin.Spec
import gwen.core.node.gherkin.SpecType
import gwen.core.node.gherkin.SpecPrinter
import gwen.core.result.SpecResult
import gwen.core.status._

import scala.util.chaining._

import java.util.Date

import com.typesafe.scalalogging.LazyLogging

/**
  * Spec evaluation engine.
  */
trait SpecEngine[T <: EvalContext] extends LazyLogging {
  engine: EvalEngine[T] =>

  private [engine] def evaluateFeature(parent: GwenNode, spec: Spec, metaResults: List[SpecResult], dataRecord: Option[DataRecord], ctx: T): SpecResult = {
    ctx.topScope.featureScope.set("gwen.feature.language", spec.feature.language)
    Dialect.withLanguage(spec.feature.language) {
      val nspec = normaliseSpec(spec, dataRecord, ctx.options)
      evaluateSpec(parent, nspec, metaResults, dataRecord, ctx)
    }
  }

  private [engine] def evaluateMeta(parent: GwenNode, meta: Spec, metaResults: List[SpecResult], dataRecord: Option[DataRecord], ctx: T): SpecResult = {
    val nmeta = normaliseSpec(meta, dataRecord, ctx.options)
    val metaResult = evaluateSpec(parent, nmeta, metaResults, dataRecord, ctx)
    val metaSpec = metaResult.spec
    metaSpec.evalStatus match {
      case _: Passed | Loaded | Skipped =>
        metaResult
      case Failed(_, error) =>
        Errors.evaluationError(s"Failed to load meta: $metaSpec: ${error.getMessage}")
      case _ =>
        Errors.evaluationError(s"Failed to load meta: $metaSpec")
    }
  }

  /**
    * Evaluates a specification.
    */
  private def evaluateSpec(parent: GwenNode, spec: Spec, metaResults: List[SpecResult], dataRecord: Option[DataRecord], ctx: T): SpecResult = {
    val specType = spec.specType
    ctx.topScope.pushObject(SpecType.toString, specType)
    try {
      beforeSpec(spec, ctx)
      val started = {
        if (spec.isMeta) {
          new Date()
        } else {
          metaResults.sortBy(_.started).headOption.map(_.started).getOrElse(new Date)
        }
      }
      ctx.topScope.initStart(started.getTime())
      (if(spec.isMeta) "Loading" else "Evaluating") tap {action =>
        logger.info("")
        logger.info(s"$action $specType: ${spec.feature.name}${spec.specFile.map(file => s" [file: $file]").getOrElse("")}")
      }
      val resultSpec = spec.copy(
        withBackground = None,
        withScenarios = evaluateScenarios(spec, spec.scenarios, dataRecord, ctx),
        withRules = evaluateRules(spec, spec.rules, dataRecord, ctx),
        withMetaSpecs = metaResults.map(_.spec)
      )
      resultSpec.specFile foreach { _ =>
        logger.info(s"${if (resultSpec.isMeta) "Loaded" else "Evaluated"} $specType: ${spec.feature.name}${spec.specFile.map(file => s" [file: $file]").getOrElse("")}")
      }
      new SpecResult(resultSpec, None, ctx.getVideos, metaResults, started, new Date()) tap { result =>
        if(!spec.isMeta) {
          logStatus(ctx.options, resultSpec)
        } else {
          StatusLogger.log(ctx.options, logger, result.evalStatus, result.toString)
        }
        afterSpec(result, ctx)
      }
    } finally {
      spec.specFile foreach { _ =>
        ctx.topScope.popObject(SpecType.toString)
      }
    }
  }

}
