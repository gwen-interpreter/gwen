/*
 * Copyright 2021-2022 Branko Juric, Brady Wood
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
import gwen.core.node.gherkin.FeatureKeyword
import gwen.core.node.gherkin.Examples
import gwen.core.node.gherkin.Annotations
import gwen.core.node.gherkin.Scenario
import gwen.core.node.gherkin.SpecNormaliser
import gwen.core.node.gherkin.Tag
import gwen.core.state.DataRecord
import gwen.core.status.Passed

import scala.util.chaining._

import com.github.tototoshi.csv.CSVReader
import com.github.tototoshi.csv.defaultCSVFormat
import com.typesafe.scalalogging.LazyLogging

import java.io.File

/**
  * Examples evaluation engine.
  */
trait ExamplesEngine[T <: EvalContext] extends SpecNormaliser with LazyLogging {
  engine: EvalEngine[T] =>

  def evaluateExamples(parent: GwenNode, examples: List[Examples], ctx: T): List[Examples] = {
    examples map { exs =>
      beforeExamples(exs, ctx)
      if (exs.scenarios.isEmpty) {
        transitionExamples(exs, Passed(0, abstained = true), ctx)
      } else {
        exs.copy(
          withScenarios = exs.scenarios map { scenario =>
            evaluateScenario(exs, scenario, ctx)
          }
        )
      } tap { exs =>
        afterExamples(exs, ctx)
      }
    }
  }

  /**
    * Loads the CSV examples for every Examples(file.csv) tag on the given outline and expands them.
    *
    * @param outline the scenario outline
    * @return a new scenario outline containing the loaded examples data
    *         or the unchanged outline if no csv data is specified or if incoming scenario is not an outline
    */
  private [engine] def expandCSVExamples(outline: Scenario, dataRecord: Option[DataRecord], ctx: T): Scenario = {
    val interpolator: String => String = dataRecord.map(_.interpolator) getOrElse { identity }
    val iTags = outline.tags map { tag => 
      Tag(tag.sourceRef, interpolateString(tag.toString) { interpolator })
    }
    val csvExamples = iTags flatMap { tag =>
      if (tag.name.startsWith(Annotations.Examples.toString)) {
        val (filepath, where) = tag.name match {
          case r"""Examples\(file="(.+?)$file",where="(.+?)$where"\)""" => (file, Some(where))
          case r"Examples" if tag.value.nonEmpty => (tag.value.get, None)
          case _ => Errors.invalidTagError(s"""Invalid Examples tag syntax: $tag - correct syntax is @Examples("path/file.csv") or @Examples(file="path/file.csv",where="name=value")""")
        }
        val examplesTag = tag.copy(withValue = Some(filepath))
        val file = new File(filepath)
        if (!file.exists()) Errors.missingOrInvalidImportFileError(examplesTag)
        if (!file.getName.toLowerCase.endsWith(".csv")) Errors.unsupportedDataFileError(examplesTag)
        val table0 = CSVReader.open(file).iterator.toList.zipWithIndex map { (row, idx) => 
          (idx + 1L, row.toList) 
        }
        val header = table0.headOption map { (_, headings) => headings }
        val table = table0 filter { (rowNo, row) => 
          if (rowNo == 1) true else {
            where map { clause => 
              clause match {
                case r"(.+?)$name=(.+?)$value" => 
                  header.map(_.indexOf(name)) map { idx => 
                    row(idx) == value
                  } getOrElse false
                case _ => true
              }
            } getOrElse true
          }
        }
        Some(Examples(None, Nil, FeatureKeyword.nameOf(FeatureKeyword.Examples), s"Data file: $filepath${where map { clause => s", where $clause"} getOrElse ""}", Nil, table, Some(file), Nil))
      } 
      else if (tag.name.equalsIgnoreCase(Annotations.Examples.toString)) {
        Errors.invalidTagError(s"""Invalid Examples tag syntax: $tag - correct syntax is @Examples("path/file.csv") or @Examples(file="path/file.csv",where="name=value")""")
      } else {
        None
      }
    }
    csvExamples match {
      case Nil => outline
      case _ =>
        val examples = normaliseScenarioOutline(
            outline.copy(withExamples = csvExamples),
            outline.background,
            dataRecord
          ).examples
        outline.copy(
          withTags = iTags,
          withName = interpolateString(outline.name) { interpolator },
          withDescription = outline.description map { line => 
            interpolateString(line) { interpolator }
          },
          withExamples = outline.examples ++ examples
        )
    }
  }

}
