/*
 * Copyright 2022-2024 Branko Juric, Brady Wood
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
import gwen.core.data.DataRecord
import gwen.core.data.DataSource
import gwen.core.eval.EvalContext
import gwen.core.eval.action.UnitStepAction
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step

import scala.util.chaining._
import scala.util.Try

import java.io.File
import gwen.core.eval.ComparisonOperator

class UniqueDataCheck[T <: EvalContext](name: String, filepath: Option[String], filepathRef: Option[String]) extends UnitStepAction[T] {
  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    step tap { _ =>
      checkStepRules(step, BehaviorType.Assertion, ctx)
      val value = ctx.getBoundValue(name)
      val file = new File(filepath.getOrElse(ctx.getBoundValue(filepathRef.get)))
      if (!file.exists()) Errors.missingFileError("Data file", file)
      val dataSource =  DataSource(file)
      val table = dataSource.table.zipWithIndex map { (row, idx) => 
        (idx + 1L, row.toList) 
      }
      val header = table.headOption map { (_, headings) => headings }
      val idx = header map { h => 
        h.indexOf(name)
      } getOrElse -1
      if (idx < 0) Errors.dataLookupError(file, name)
      val matches = table.tail filter { (rowNo, row) =>   
        val rec = DataRecord(dataSource, Occurrence(rowNo.toInt - 1, table.size - 1), header.get zip row)
        rec.data.exists { (n, v) => 
          n.trim.toUpperCase == name.trim.toUpperCase && value.trim == v.trim
        }
      }
      ctx.assertWithError(
          matches.size < 2, 
          s"$name $value is not unique in $file file (${matches.size} occurrences found)",
          step.assertionMode)
    }
  }

}

