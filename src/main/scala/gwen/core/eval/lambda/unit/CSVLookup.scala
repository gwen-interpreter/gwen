/*
 * Copyright 2022 Branko Juric, Brady Wood
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

package gwen.core.eval.lambda.unit

import gwen.core._
import gwen.core.behavior.BehaviorType
import gwen.core.eval.EvalContext
import gwen.core.eval.lambda.UnitStep
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step
import gwen.core.state.DataRecord

import com.github.tototoshi.csv.CSVReader
import com.github.tototoshi.csv.defaultCSVFormat

import scala.util.chaining._
import scala.util.Try

import java.io.File

class CSVLookup[T <: EvalContext](column: String, name: String, filepath: Option[String], filpathRef: Option[String], jsPredicate: String) extends UnitStep[T] {
  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    step tap { _ =>
      checkStepRules(step, BehaviorType.Action, ctx)
      val file = new File(filepath.getOrElse(s"${ctx.getBoundReferenceValue(s"${filpathRef.get} file")}"))
      if (FileIO.isCsvFile(file)) {
        if (!file.exists()) Errors.missingFileError(file)
        val records = CSVRecords.list(file)
        val table = records.zipWithIndex map { (row, idx) => 
          (idx + 1L, row.toList) 
        }
        val header = table.headOption map { (_, headings) => headings map { h => s"${CSVRecords.lookupPrefix}$h" } }
        val idx = header.map(_.indexOf(s"${CSVRecords.lookupPrefix}$column")).getOrElse(-1)
        if (idx < 0) Errors.csvLookupError(file, column)
        val record = table.tail find { (rowNo, row) =>   
          val dataRecord = DataRecord(file, rowNo.toInt - 1, table.size - 1, header.get zip row)
          val js0 = dataRecord.interpolateStrict(jsPredicate)
          val js1 = ctx.interpolateParams(js0)
          val javascript = ctx.interpolate(js1)
          val raw = ctx.evaluate("true") {
            Option(ctx.evaluateJS(ctx.formatJSReturn(javascript))).map(_.toString).getOrElse("false")
          }
          Try(raw.toBoolean).getOrElse(Errors.invalidTypeError(s"Boolean expected but got '$raw' when evaluating JS predicate: $javascript"))
        }
        record foreach { (_, r) => 
          val value = r(idx)
          ctx.scopes.topScope.set(name, value)
        }
      } else {
        Errors.unsupportedLookupFileError(file, "*.csv")
      }
    }
  }

}

