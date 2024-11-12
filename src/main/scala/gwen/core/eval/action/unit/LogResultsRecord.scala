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

package gwen.core.eval.action.unit

import gwen.core._
import gwen.core.behavior.BehaviorType
import gwen.core.eval.EvalContext
import gwen.core.eval.action.UnitStepAction
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step
import gwen.core.result.ResultFile
import gwen.core.report.ReportFormat
import gwen.core.status.Passed

import java.io.File

class LogResultsRecord[T <: EvalContext](resultsFileId: String) extends UnitStepAction[T] {
  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    val start = System.nanoTime
    checkStepRules(step, BehaviorType.Action, ctx)
    val resultsFile = ctx.options.resultFiles.find(_.id == resultsFileId) getOrElse {
      Errors.resultsFileError(s"No such result file: gwen.reports.results.files.$resultsFileId setting not found")
    }
    if (resultsFile.scope.nonEmpty) {
       Errors.resultsFileError(s"Scope not permitted on results file when logging with step${Errors.at(step.sourceRef)} - (remove gwen.report.results.files.${resultsFile.id}.scope setting or don't use step DSL)")
    }
    if (resultsFile.logRecord(parent, ctx, ctx.options)) {
      step
    } else {
      step.copy(withEvalStatus = Passed(System.nanoTime - start, abstained = true))
    }
  }

}
