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

import gwen.core.Errors
import gwen.core.engine.EvalContext
import gwen.core.engine.EvalEngine
import gwen.core.engine.SpecNormaliser
import gwen.core.model._
import gwen.core.model.node._

import scala.util.Success
import scala.util.Failure

import com.typesafe.scalalogging.LazyLogging
import io.cucumber.gherkin.ParserException

import java.io.File

/**
  * Feature unit evaluation engine
  */
trait UnitEngine[T <: EvalContext]
  extends SpecEngine[T] with BackgroundEngine[T] with RuleEngine[T] with ScenarioEngine[T] with ExamplesEngine[T] 
  with StepDefEngine[T] with StepEngine[T] with GherkinParser with SpecNormaliser with LazyLogging {
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
    Option(unit.featureFile).filter(_.exists()) map { file =>
      parseSpec(file) match {
        case Success(pspec) =>
          unit.tagFilter.filter(pspec) map { spec =>
            evaluateSpec(unit, spec, loadedMeta, ctx)
          }
        case Failure(e) =>
          e match {
            case pe: ParserException => Errors.syntaxError(unit.uri, pe)
            case _ => Errors.syntaxError(e.getMessage)
          }
      }
    } getOrElse {
      logger.warn(s"Skipped missing feature file: ${unit.featureFile.getPath}") 
      None
    }
  }

  private def evaluateSpec(unit: FeatureUnit, spec: Spec, loadedMeta: List[File], ctx: T): SpecResult = {
    unit.dataRecord foreach { rec =>
      ctx.topScope.set("data record number", rec.recordNo.toString)
    }
    val unitMeta = loadMetaFiles(unit, unit.metaFiles, loadedMeta, ctx)
    val unitMetaFiles = unitMeta.flatMap(_.spec.specFile)
    val importMeta = loadMetaFiles(unit, metaImportFiles(spec, unit.featureFile), loadedMeta ++ unitMetaFiles, ctx)
    val metaResults = unitMeta ++ importMeta
    if (spec.isMeta) {
      evaluateMeta(unit, spec, metaResults, unit.dataRecord, ctx)
    } else {
      beforeUnit(unit, ctx.scopes)
      evaluateFeature(unit, spec, metaResults, unit.dataRecord, ctx) tap { result => 
        afterUnit(FeatureUnit(unit.ancestor, unit, result), ctx.scopes)
      }
    }
  }

  private def loadMetaFiles(unit: FeatureUnit, metaFiles: List[File], loadedMeta: List[File], ctx: T): List[SpecResult] = {
    metaFiles filter { file => 
      !loadedMeta.contains(file)
    } flatMap { file => 
      val metaUnit = FeatureUnit(unit, file, Nil, None, unit.tagFilter)
      evaluateUnit(metaUnit, loadedMeta, ctx)
    }
  }

  private def metaImportFiles(spec: Spec, specFile: File): List[File] = {
    spec.feature.tags.flatMap { tag =>
      tag match {
        case Tag(_, name, Some(filepath)) =>
          if (name == ReservedTags.Import.toString) {
            val file = new File(filepath)
            if (!file.exists()) Errors.missingOrInvalidImportFileError(tag)
            if (!file.getName.endsWith(".meta")) Errors.unsupportedImportError(tag)
            if (file.getCanonicalPath.equals(specFile.getCanonicalPath)) {
              Errors.recursiveImportError(tag)
            }
            Some(file)
          } else if (name.equalsIgnoreCase(ReservedTags.Import.toString)) {
            Errors.invalidTagError(s"""Invalid import syntax: $tag - correct syntax is @Import("filepath")""")
          } else {
            None
          }
        case _ => None
      }
    }
  }
  
}
