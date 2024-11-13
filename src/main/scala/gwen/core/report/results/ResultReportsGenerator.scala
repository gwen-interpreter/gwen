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
import gwen.core.result.ResultFile
import gwen.core.result.SpecResult
import gwen.core.state.Environment
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

  val resultFiles: List[ResultFile] = options.resultFiles
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
    val resources = resultFiles.filter(_.file.exists).map(_.file.getCanonicalPath)
    val error = if (errors.nonEmpty) Some(new Errors.ResultsFileException(errors)) else None
    ReportResult(ReportFormat.results, resources, error)
  }

  override def afterSpec(event: NodeEvent[SpecResult]): Unit = {
    val result = event.source 
    candidateFiles(result.spec, result.spec.feature.tags) foreach { resFile => 
      report(resFile, result.spec, event.env)
    }
  }

  override def afterRule(event: NodeEvent[Rule]): Unit = { 
    val rule = event.source
    if (rule.scenarios.nonEmpty) {
      candidateFiles(rule, rule.tags) foreach { resFile => 
        report(resFile, rule, event.env)
      }
    }
  }
  
  override def afterScenario(event: NodeEvent[Scenario]): Unit = { 
    val scenario = event.source
    if (!scenario.isOutline && scenario.steps.nonEmpty) {
      candidateFiles(scenario, scenario.tags) foreach { resFile => 
        report(resFile, scenario, event.env)
      }
    }
  }

  override def afterExamples(event: NodeEvent[Examples]): Unit = { 
    val examples = event.source
    if (examples.scenarios.nonEmpty) {
      candidateFiles(examples, examples.tags) foreach { resFile => 
        report(resFile, examples, event.env)
      }
    }
  }

  override def afterStepDef(event: NodeEvent[Scenario]): Unit = { 
    val stepDef = event.source
    if (stepDef.isStepDefCall) {
      candidateFiles(stepDef, stepDef.tags) foreach { resFile => 
        report(resFile, stepDef, event.env)
      }
    }
  }

  private def candidateFiles(node: GwenNode, tags: List[Tag]): List[ResultFile] = {
    val annotatedIds = ResultFile.parseAnnotation(tags, resultFiles, node.nodeType)
    val scopedIds = resultFiles.filter(f => f.scope.nonEmpty).map(_.id)
    val ids = (annotatedIds ++ scopedIds).distinct
    resultFiles.filter(resFile => ids.contains(resFile.id))
  }

  private def report(resFile: ResultFile, node: GwenNode, env: Environment): Unit = {
    try {
      resFile.logRecord(node, env, options)
    } catch {
      case e: Errors.ResultsFileException => e.errors.foreach(addError)
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
