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

package gwen.core.engine.spec

import gwen.core._
import gwen.core.engine.EvalContext
import gwen.core.engine.EvalEngine
import gwen.core.model._
import gwen.core.model.gherkin.Dialect
import gwen.core.model.gherkin.Spec
import gwen.core.model.prettyPrint

import java.util.Date

import com.typesafe.scalalogging.LazyLogging

/**
  * Spec evaluation engine.
  */
trait SpecEngine[T <: EvalContext] extends LazyLogging {
    engine: EvalEngine[T] =>

  private [spec] def evaluateFeature(parent: Identifiable, spec: Spec, metaResults: List[SpecResult], dataRecord: Option[DataRecord], ctx: T): SpecResult = {
    spec.specFile foreach { file =>
      ctx.topScope.set("gwen.feature.file.name", file.getName)
      ctx.topScope.set("gwen.feature.file.path", file.getPath)
      ctx.topScope.set("gwen.feature.file.absolutePath", file.getAbsolutePath)
    }
    ctx.topScope.set("gwen.feature.name", spec.feature.name)
    Dialect.withLanguage(spec.feature.language) {
      val nspec = normaliseSpec(spec, spec.specFile, dataRecord)
      evaluateSpec(parent, nspec, metaResults, ctx)
    }
  }

  private [spec] def evaluateMeta(parent: Identifiable, meta: Spec, metaResults: List[SpecResult], dataRecord: Option[DataRecord], ctx: T): SpecResult = {
    val nmeta = normaliseSpec(meta, meta.specFile, dataRecord)
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
  private def evaluateSpec(parent: Identifiable, spec: Spec, metaResults: List[SpecResult], ctx: T): SpecResult = {
    val specType = spec.specType
    ctx.topScope.pushObject(SpecType.toString, specType)
    try {
      beforeSpec(parent, spec, ctx.scopes)
      val started = new Date()
      (if(spec.isMeta) "Loading" else "Evaluating") tap {action =>
        logger.info("")
        logger.info(s"$action $specType: ${spec.feature.name}${spec.specFile.map(file => s" [file: $file]").getOrElse("")}")
      }
      val resultSpec = spec.copy(
        withBackground = None,
        withScenarios = evaluateScenarios(spec, spec.scenarios, ctx),
        withRules = evaluateRules(spec, spec.rules, ctx),
        withMetaSpecs = metaResults.map(_.spec)
      )
      resultSpec.specFile foreach { _ =>
        logger.info(s"${if (resultSpec.isMeta) "Loaded" else "Evaluated"} $specType: ${spec.feature.name}${spec.specFile.map(file => s" [file: $file]").getOrElse("")}")
      }
      logger.debug(prettyPrint(resultSpec))
      new SpecResult(resultSpec, None, metaResults, started, new Date()) tap { result =>
        if(!spec.isMeta) {
          logStatus(resultSpec)
        } else {
          logger.info(result.toString)
        }
        afterSpec(result, ctx.scopes)
      }
    } finally {
      spec.specFile foreach { _ =>
        ctx.topScope.popObject(SpecType.toString)
      }
    }
  }

}