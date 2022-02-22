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
import gwen.core.node._
import gwen.core.node.gherkin.Dialect
import gwen.core.node.gherkin.Spec
import gwen.core.node.gherkin.SpecType
import gwen.core.node.gherkin.SpecPrinter
import gwen.core.result.SpecResult
import gwen.core.state.DataRecord
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
    spec.specFile foreach { file =>
      ctx.topScope.set("gwen.feature.file.name", file.getName)
      ctx.topScope.set("gwen.feature.file.path", file.getPath)
      ctx.topScope.set("gwen.feature.file.absolutePath", file.getAbsolutePath)
    }
    ctx.topScope.set("gwen.feature.name", spec.feature.name)
    ctx.topScope.set("gwen.eval.status.keyword", StatusKeyword.Pending.toString)
    ctx.topScope.set("gwen.eval.status.message", "")
    Dialect.withLanguage(spec.feature.language) {
      val nspec = normaliseSpec(spec, dataRecord)
      evaluateSpec(parent, nspec, metaResults, ctx)
    }
  }

  private [engine] def evaluateMeta(parent: GwenNode, meta: Spec, metaResults: List[SpecResult], dataRecord: Option[DataRecord], ctx: T): SpecResult = {
    val nmeta = normaliseSpec(meta, dataRecord)
    val metaResult = evaluateSpec(parent, nmeta, metaResults, ctx)
    val metaSpec = metaResult.spec
    metaSpec.evalStatus match {
      case Passed(_) | Loaded =>
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
  private def evaluateSpec(parent: GwenNode, spec: Spec, metaResults: List[SpecResult], ctx: T): SpecResult = {
    val specType = spec.specType
    ctx.topScope.pushObject(SpecType.toString, specType)
    try {
      beforeSpec(spec, ctx)
      val started = new Date()
      (if(spec.isMeta) "Loading" else "Evaluating") tap {action =>
        logger.info("")
        logger.info(s"$action $specType: ${spec.feature.name}${spec.specFile.map(file => s" [file: $file]").getOrElse("")}")
      }
      val resultSpec = spec.copy(
        withBackground = None,
        withScenarios = evaluateScenarios(spec, spec.scenarios, ctx, spec.feature.language),
        withRules = evaluateRules(spec, spec.rules, ctx, spec.feature.language),
        withMetaSpecs = metaResults.map(_.spec)
      )
      resultSpec.specFile foreach { _ =>
        logger.info(s"${if (resultSpec.isMeta) "Loaded" else "Evaluated"} $specType: ${spec.feature.name}${spec.specFile.map(file => s" [file: $file]").getOrElse("")}")
      }
      new SpecResult(resultSpec, None, metaResults, started, new Date()) tap { result =>
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
