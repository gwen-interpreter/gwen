/*
 * Copyright 2024 Branko Juric, Brady Wood
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

package gwen.core.report.results

import gwen.core._
import gwen.core.GwenInfo
import gwen.core.GwenOptions
import gwen.core.GwenSettings
import gwen.core.node._
import gwen.core.node.event._
import gwen.core.node.gherkin._
import gwen.core.report.NoopReportGenerator
import gwen.core.report.ReportFormat
import gwen.core.report.ReportResult
import gwen.core.result.SpecResult
import gwen.core.state.ScopedDataStack
import gwen.core.status.EvalStatus

import com.typesafe.scalalogging.LazyLogging

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import java.io.File

/**
  * Generates results files.
  * 
  * @author Branko Juric
  */
class ResultReportsGenerator(options: GwenOptions, info: GwenInfo) 
    extends NoopReportGenerator(ResultReportsConfig, options, info) 
    with NodeEventListener("Results Reporter") 
    with LazyLogging with ImplicitValueKeys {

  val resultFiles: List[ResultFile] = GwenSettings.`gwen.report.results.files`(options)
  var errors: List[String] = Nil

  override def init(lifecycle: NodeEventDispatcher): Unit = { 
    lifecycle.addListener(this)
    resultFiles foreach { resFile => 
      val header = resFile.fields.map(_._1).map(Formatting.escapeNewLineChars).map(Formatting.escapeCSV).mkString(",")
      resFile.file.writeLine(header)
    }
  }

  override def close(lifecycle: NodeEventDispatcher, evalStatus: EvalStatus): ReportResult = { 
    lifecycle.removeListener(this)
    val resources = resultFiles.map(_.file.getCanonicalPath)
    val error = if (errors.nonEmpty) Some(new Errors.CSVResultsReferenceException(errors)) else None
    ReportResult(ReportFormat.results, resources, error)
  }

  override def afterSpec(event: NodeEvent[SpecResult]): Unit = {
    val result = event.source 
    filterFiles(result.spec, result.spec.feature.tags) foreach { resFile => 
      report(resFile, result.evalStatus, event.scopes)
    }
  }

  override def afterRule(event: NodeEvent[Rule]): Unit = { 
    val rule = event.source
    if (rule.scenarios.nonEmpty) {
      filterFiles(rule, rule.tags) foreach { resFile => 
        report(resFile, rule.evalStatus, event.scopes)
      }
    }
  }
  
  override def afterScenario(event: NodeEvent[Scenario]): Unit = { 
    val scenario = event.source
    if (!scenario.isOutline && scenario.steps.nonEmpty) {
      filterFiles(scenario, scenario.tags) foreach { resFile => 
        report(resFile, scenario.evalStatus, event.scopes)
      }
    }
  }

  override def afterExamples(event: NodeEvent[Examples]): Unit = { 
    val examples = event.source
    if (examples.scenarios.nonEmpty) {
      filterFiles(examples, examples.tags) foreach { resFile => 
        report(resFile, examples.evalStatus, event.scopes)
      }
    }
  }

  override def afterStepDef(event: NodeEvent[Scenario]): Unit = { 
    val stepDef = event.source
    if (!stepDef.evalStatus.isLoaded && !stepDef.isOutline && !stepDef.isSynthetic && stepDef.steps.nonEmpty) {
      filterFiles(stepDef, stepDef.tags) foreach { resFile => 
        report(resFile, stepDef.evalStatus, event.scopes)
      }
    }
  }

  private def filterFiles(node: GwenNode, tags: List[Tag]): List[ResultFile] = {
    val annotatedIds = tags.filter(_.name == Annotations.Results.toString) flatMap { annotation =>
      annotation.value flatMap { id =>
        if (resultFiles.filter(_.id == id).isEmpty) {
          addError(s"gwen.reports.results.files.$id setting not found for results file denoted by id in $annotation annotation${Errors.at(annotation.sourceRef)}")
          None
        } else {
          Some(id)
        }
      }
    }
    val scopedIds = resultFiles filter { resFile => 
      resFile.scope.map(_.nodeType) map { resFileNodeType =>
        if (resFileNodeType == node.nodeType) {
          resFile.scope.map(_.nodeName.map(n => node.name.matches("(.* -- )?" + n + "( -- .*)?$")).getOrElse(true)).getOrElse(true)
        } else {
          false
        }
      } getOrElse false
    } map(_.id)
    val ids = (annotatedIds ++ scopedIds).distinct
    resultFiles.filter(resFile => ids.contains(resFile.id))
  }

  private def report(resFile: ResultFile, evalStatus: EvalStatus, scopes: ScopedDataStack): Unit = {
    if (resFile.status.map(_ == evalStatus.keyword).getOrElse(true)) {
      val record = resFile.fields map { field =>
        val value = {
          scopes.getOpt(field.ref).getOrElse {
            if (field.optional) "" else { 
              addError(s"Unbound ${field.name} field ref ${field.ref} in file: ${resFile.file}")
              s"Unbound ref: ${field.ref}"
            }
          }
        }
        if (value.trim != "") Formatting.escapeCSV(Formatting.escapeNewLineChars(value))
        else value
      }
      resFile.synchronized {
        resFile.file.appendLine(record.mkString(","))
      }
    }
  }

  private def addError(error: String): Unit = {
    errors.synchronized {
      if (!errors.contains(error)) {
        errors = (errors ++ List(error))
      }
    }
  }

}