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

import gwen.core.Errors
import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.node._
import gwen.core.node.gherkin._
import gwen.core.status.Pending
import gwen.core.result.SpecResult

import scala.util.Success
import scala.util.Failure
import scala.util.chaining._

import com.typesafe.scalalogging.LazyLogging
import io.cucumber.gherkin.ParserException

import java.io.File

/**
  * Feature unit evaluation engine
  */
trait UnitEngine[T <: EvalContext]
    extends SpecEngine[T]
    with BackgroundEngine[T]
    with RuleEngine[T]
    with ScenarioEngine[T]
    with ExamplesEngine[T]
    with StepDefEngine[T]
    with StepEngine[T]
    with GherkinParser
    with SpecNormaliser
    with LazyLogging {
  engine: EvalEngine[T] =>

  /**
    * Interprets a feature unit.
    *
    * @param unit the feature unit to process
    * @param ctx the evaluation context
    */
  def evaluateUnit(unit: FeatureUnit, ctx: T): Option[SpecResult] = {
    evaluateUnit(unit, Nil, ctx)
  }

  /**
    * Interprets a feature unit. Recursively loads all meta first followed by feature.
    *
    * @param unit the feature unit to process
    * @param loadedMeta cumulative meta files
    * @param ctx the evaluation context
    */
  private def evaluateUnit(unit: FeatureUnit, loadedMeta: List[File], ctx: T): Option[SpecResult] = {
    Option(unit.featureFile).filter(f => f.isFile && f.exists()) map { file =>
      parseSpec(file) match {
        case Success(pspec) =>
          unit.tagFilter.filter(pspec) match {
            case Some(spec) =>
              Some(evaluateSpec(unit, spec, loadedMeta, ctx))
            case None =>
              if (ctx.options.verbose)
                logger.info(s"Feature file skipped (does not satisfy tag filters): ${file}")
              else 
                println(s"Feature file skipped (does not satisfy tag filters): ${file}\n")
              None
          }
        case Failure(e) =>
          e match {
            case pe: Errors.ParserException => Errors.syntaxError(unit.featureFile, pe.parseError)
            case _ => Errors.syntaxError(e.getMessage)
          }
      }
    } getOrElse {
      if (unit.featureFile.isFile) {
        logger.warn(s"Skipped missing feature file: ${unit.featureFile.getPath}")
      }
      None
    }
  }

  private def evaluateSpec(unit: FeatureUnit, spec: Spec, loadedMeta: List[File], ctx: T): SpecResult = {
    ctx.topScope.setImplicitAtts(Some(spec), Pending)
    unit.dataRecord foreach { rec =>
      ctx.topScope.set("data record number", rec.recordNo.toString)
      ctx.topScope.set("data.record.number", rec.recordNo.toString)
      ctx.topScope.set("data.record.index", (rec.recordNo - 1).toString)
    }
    val unitMeta = loadMetaFiles(unit, unit.metaFiles, loadedMeta, ctx)
    val unitMetaFiles = unitMeta.flatMap(_.spec.specFile)
    val importMeta = loadMetaFiles(unit, metaImportFiles(spec, unit.featureFile), loadedMeta ++ unitMetaFiles, ctx)
    val metaResults = unitMeta ++ importMeta
    if (spec.isMeta) {
      evaluateMeta(unit, spec, metaResults, unit.dataRecord, ctx)
    } else {
      beforeUnit(unit, ctx)
      evaluateFeature(unit, spec, metaResults, unit.dataRecord, ctx) tap { result =>
        afterUnit(FeatureUnit(unit.ancestor, unit, result), ctx)
      }
    }
  }

  private def loadMetaFiles(unit: FeatureUnit, metaFiles: List[File], loadedMeta: List[File], ctx: T): List[SpecResult] = {
    metaFiles filter { file =>
      !loadedMeta.contains(file)
    } flatMap { file =>
      val metaUnit = FeatureUnit(unit, file, Nil, unit.dataRecord, unit.tagFilter)
      evaluateUnit(metaUnit, loadedMeta, ctx)
    }
  }

  private def metaImportFiles(spec: Spec, specFile: File): List[File] = {
    spec.feature.tags.flatMap { tag =>
      tag match {
        case Tag(_, name, Some(filepath)) =>
          if (name == Annotations.Import.toString) {
            val file = new File(filepath)
            if (!file.exists()) Errors.missingOrInvalidImportFileError(tag)
            if (!file.getName.endsWith(".meta")) Errors.unsupportedImportError(tag)
            if (file.getCanonicalPath.equals(specFile.getCanonicalPath)) {
              Errors.recursiveImportError(tag)
            }
            Some(file)
          } else if (name.equalsIgnoreCase(Annotations.Import.toString)) {
            Errors.invalidTagError(s"""Invalid import syntax: $tag - correct syntax is @Import("filepath")""")
          } else {
            None
          }
        case _ => None
      }
    }
  }

}
