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
import gwen.core.data.DataRecord
import gwen.core.data.DataSource
import gwen.core.eval.EvalContext
import gwen.core.eval.lambda.UnitStep
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step

import scala.util.chaining._
import scala.util.Try

import java.io.File

class DataLookup[T <: EvalContext](dataName: String, name: String, filepath: Option[String], filepathRef: Option[String], jsPredicate: String) extends UnitStep[T] {
  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    step tap { _ =>
      checkStepRules(step, BehaviorType.Action, ctx)
      val file = new File(filepath.getOrElse(s"${ctx.getBoundValue(s"${filepathRef.get} file")}"))
      if (!file.exists()) Errors.missingFileError(file)
      val dataSource =  DataSource(file)
      val table = dataSource.table.zipWithIndex map { (row, idx) => 
        (idx + 1L, row.toList) 
      }
      val header = table.headOption map { (_, headings) => headings map { h => s"${DataSource.lookupPrefix}$h" } }
      val idx = header map { h => 
        h.indexOf(s"${DataSource.lookupPrefix}$dataName")
      } getOrElse -1
      if (idx < 0) Errors.dataLookupError(file, dataName)
      val record = table.tail find { (rowNo, row) =>   
        val dataRecord = DataRecord(dataSource, rowNo.toInt - 1, table.size - 1, header.get zip row)
        try {
          val js0 = dataRecord.interpolateStrict(jsPredicate.replace(dataSource.lookupPrefix, DataSource.lookupPrefix))
          val js1 = ctx.interpolateParams(js0)
          val javascript = ctx.interpolate(js1)
          val raw = ctx.evaluate("true") {
            Option(ctx.evaluateJS(javascript)).map(_.toString).getOrElse("false")
          }
          Try(raw.toBoolean).getOrElse(Errors.invalidTypeError(s"Boolean expected but got '$raw' when evaluating JS predicate: $javascript"))
        } catch {
          case e: Errors.UnboundAttributeException => 
            val name = e.name
            if (jsPredicate.contains(s"$${$name}")) throw e
            else {
              val n = {
                if (name.startsWith(DataSource.lookupPrefix)) name.replace(DataSource.lookupPrefix, dataSource.lookupPrefix)
                else if (name.startsWith(dataSource.lookupPrefix)) name.replace(dataSource.lookupPrefix, DataSource.lookupPrefix)
                else throw e
              }
              Errors.unboundAttributeError(n, e.scope, e.cause)
            }
        }
      }
      record foreach { (_, r) => 
        val value = r(idx)
        ctx.scopes.topScope.set(name, value)
      }
    }
  }

}

