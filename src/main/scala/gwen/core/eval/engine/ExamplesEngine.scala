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
import gwen.core.eval.support.JSCondition
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.FeatureKeyword
import gwen.core.node.gherkin.Examples
import gwen.core.node.gherkin.Annotations
import gwen.core.node.gherkin.Background
import gwen.core.node.gherkin.Scenario
import gwen.core.node.gherkin.SpecNormaliser
import gwen.core.node.gherkin.Step
import gwen.core.node.gherkin.StepKeyword
import gwen.core.node.gherkin.Tag
import gwen.core.state.DataRecord
import gwen.core.status.Passed
import gwen.core.status.Pending

import scala.util.chaining._

import com.typesafe.scalalogging.LazyLogging

import java.io.File
import gwen.core.eval.binding.BindingType

/**
  * Examples evaluation engine.
  */
trait ExamplesEngine[T <: EvalContext] extends SpecNormaliser with LazyLogging {
  engine: EvalEngine[T] =>

  def evaluateExamples(parent: GwenNode, examples: List[Examples], ctx: T): List[Examples] = {
    examples map { exs =>
      beforeExamples(exs, ctx)
      if (exs.scenarios.isEmpty) {
        transitionExamples(exs, Passed(0, abstained = !ctx.options.dryRun), ctx)
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
    val interpolator = DataRecord.interpolateLenient(dataRecord)
    val iTags = outline.tags map { tag => 
      val iValue0 = interpolator.apply(tag.toString)
      val iValue1 = ctx.interpolateLenient(iValue0)
      Tag(tag.sourceRef, iValue1)
    }
    val csvExamplesAndBackgrouds = iTags flatMap { tag =>
      if (tag.name.startsWith(Annotations.Examples.toString)) {
        val (filepath, prefix, where, required) = tag.name match {
          case r"""Examples\(file="(.+?)$file",prefix="(.+?)$prefix",where="(.+?)$where",required=(true|false)$required\)""" => (file, Some(prefix), Some(where), required.toBoolean)
          case r"""Examples\(file="(.+?)$file",prefix="(.+?)$prefix",where="(.+?)$where"\)""" => (file, Some(prefix), Some(where), false)
          case r"""Examples\(file="(.+?)$file",prefix="(.+?)$prefix",required=(true|false)$required\)""" => (file, Some(prefix), None, required.toBoolean)
          case r"""Examples\(file="(.+?)$file",where="(.+?)$where",required=(true|false)$required\)""" => (file, None, Some(where), required.toBoolean)
          case r"""Examples\(file="(.+?)$file",where="(.+?)$where"\)""" => (file, None, Some(where), false)
          case r"""Examples\(file="(.+?)$file",prefix="(.+?)$prefix",required=(true|false)$required\)""" => (file, Some(prefix), None, required.toBoolean)
          case r"""Examples\(file="(.+?)$file",prefix="(.+?)$prefix"\)""" => (file, Some(prefix), None, false)
          case r"""Examples\(file="(.+?)$file",required=(true|false)$required\)""" => (file, None, None, required.toBoolean)
          case r"Examples" if tag.value.nonEmpty => (tag.value.get, None, None, false)
          case _ => Errors.invalidTagError(s"""Invalid Examples tag syntax: $tag - correct syntax is @Examples("path/file.csv"[,where="javascript expression"][,required=true|false])""")
        }
        val whereFilter = where.map(ctx.interpolateParams).map(ctx.interpolateLenient)
        val examplesTag = tag.copy(withValue = Some(filepath))
        val file = new File(filepath)
        if (!file.exists()) Errors.missingOrInvalidImportFileError(examplesTag)
        if (!FileIO.isCsvFile(file)) Errors.unsupportedDataFileError(examplesTag)
        val allRecords = CSVRecords.iterator(file).toList
        val records = if(ctx.options.dryRun) {
          val limit = GwenSettings.`gwen.dryRun.limit.tableData.outline.examples.records`
          if (limit == Int.MaxValue) allRecords
          else allRecords.take(limit + 1) // + 1 to include header record
        } else {
          allRecords
        }
        val table0 = records.zipWithIndex map { (row, idx) => 
          (idx + 1L, row.toList) 
        }
        val header = table0.headOption map { (_, headings) => headings map { h => s"${prefix map { p => s"$p$h" } getOrElse h }" } } getOrElse {
          Errors.csvHeaderNotFoundError(file)
        }
        val resultTable = (1L, header) :: (table0.tail filter { (rowNo, row) => 
          whereFilter map { js => 
            val dataRec = DataRecord(file, rowNo.toInt - 1, table0.size - 1, header zip row)
            val js0 = dataRec.interpolate(js)
            val js1 = ctx.interpolateParams(js0)
            val javascript = ctx.interpolate(js1)
            (ctx.evaluate("true") {
              Option(ctx.evaluateJS(ctx.formatJSReturn(javascript))).map(_.toString).getOrElse("false")
            }).toBoolean
          } getOrElse true
        })
        val (finalTable, background) = if (resultTable.size < 2 && required) {
          val msg = s"No data record(s) found in ${file.getName}${whereFilter.map(w => s" where $w").getOrElse("")}"
          csvTableFail(msg, header, outline)
        } else {
          (resultTable, None)
        }
        Some((Examples(None, Nil, FeatureKeyword.nameOf(FeatureKeyword.Examples), s"Data file: $filepath${prefix map { p => s", prefix: $p"} getOrElse ""}${whereFilter map { clause => s", where: $clause"} getOrElse ""}", Nil, finalTable, Some(file), Nil), background))
      } 
      else if (tag.name.equalsIgnoreCase(Annotations.Examples.toString)) {
        Errors.invalidTagError(s"""Invalid Examples tag syntax: $tag - correct syntax is @Examples("path/file.csv") or @Examples(file="path/file.csv",where="javascript expression")""")
      } else {
        None
      }
    }
    csvExamplesAndBackgrouds match {
      case Nil => outline
      case _ =>
        val csvExamples = csvExamplesAndBackgrouds map { (examples, _) => examples }
        val backgrounds = csvExamplesAndBackgrouds map { (_, background) => background }
        val examples = csvExamples zip backgrounds map { (exs, background) => 
          normaliseScenarioOutline(
            outline.copy(withExamples = List(exs)),
            background,
            dataRecord
          )
        } flatMap (_.examples)
        outline.copy(
          withTags = iTags,
          withName = interpolator.apply(outline.name),
          withDescription = outline.description map { interpolator },
          withExamples = outline.examples ++ examples
        )
    }
  }

  private def csvTableFail(msg: String, header: List[String], outline: Scenario): (List[(Long, List[String])], Option[Background]) = {
    val emptyTable = (1L, header) :: List((2L, header map { _ => "" }))
    val step = Step(None, StepKeyword.Given.toString, s"""${header.headOption.getOrElse("Data")} should be defined""", Nil, None, Nil, None, Pending, Nil, Nil, List(Tag(Annotations.NoData)), Some(msg))
    val noDataBackground = outline.background map { bg =>
      bg.copy(
        withName = s"${bg.name} + No data",
        withSteps = step :: bg.steps
      )
    } getOrElse {
      Background(None, Background.toString, "No data", Nil, List(step))
    }
    (emptyTable, Some(noDataBackground))
  }

}
