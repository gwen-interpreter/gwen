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

package gwen.eval

import gwen._
import gwen.model._
import gwen.model.gherkin._

import java.io.File

/**
  * Loads Gwen meta.
  * 
  * @param env the evaluation environment
  */
trait MetaLoader[T <: EvalContext] {
  interpreter: GwenInterpreter[T] =>
  
  /**
    * Loads the meta.
    * 
    * @param metaFiles the meta files to load
    * @param tagFilters user provided tag filters (includes:(tag, true) and excludes:(tag, false))
    */
  private[eval] def loadMeta(unit: Option[FeatureUnit], metaFiles: List[File], tagFilters: List[(Tag, Boolean)], ctx: T): List[FeatureResult] = {
    metaFiles.flatMap(metaFile => loadMetaFile(unit, metaFile, tagFilters, ctx))
  }

  /**
    * Loads meta imports (defned in @Import tags)
    */
  private[eval] def loadMetaImports(unit: FeatureUnit, featureSpec: Specification, tagFilters: List[(Tag, Boolean)], ctx: T): List[FeatureResult] =
    findMetaImports(featureSpec, unit.featureFile) flatMap { metaFile => 
      try {
        loadMetaFile(Some(unit), metaFile, tagFilters, ctx)
      } catch {
        case _: StackOverflowError =>
          Errors.recursiveImportError(Tag(ReservedTags.Import, unit.featureFile.getPath))
      }
    }
  
  private def findMetaImports(featureSpec: Specification, specFile: File): List[File] = {
    featureSpec.feature.tags.flatMap { tag =>
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
  
  private def loadMetaFile(unit: Option[FeatureUnit], metaFile: File, tagFilters: List[(Tag, Boolean)], ctx: T): Option[FeatureResult] = ctx.withEnv { env =>
    if (!env.loadedMeta.contains(metaFile)) {
      val metaUnit = FeatureUnit(unit.getOrElse(Root), metaFile, Nil, None)
      interpreter.interpretFeature(metaUnit, tagFilters, ctx) tap {
        case Some(metaResult) =>
          val meta = metaResult.spec
          meta.evalStatus match {
            case Passed(_) | Loaded =>
              env.loadedMeta = meta.featureFile.get :: env.loadedMeta
            case Failed(_, error) =>
              Errors.evaluationError(s"Failed to load meta: $meta: ${error.getMessage}")
            case _ =>
              Errors.evaluationError(s"Failed to load meta: $meta")
          }
        case _ => Nil
      }
    } else None
  }
  
}
