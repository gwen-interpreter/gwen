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

package gwen.core.eval.action.composite

import gwen.core.data.JsonDataSource
import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.data.DataRecord
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Annotations
import gwen.core.node.gherkin.Step
import gwen.core.node.gherkin.Tag
import gwen.core.node.gherkin.StepKeyword
import gwen.core.node.gherkin.table.DataTable
import gwen.core.state.ScopedData

import scala.io.Source
import scala.util.Success
import scala.util.Try


class ForEachJsonArrayElement[T <: EvalContext](doStep: String, entry: String, source: String, engine: EvalEngine[T]) extends ForEach[T](engine, doStep) {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    val sourceValue = ctx.getBoundValue(source)
    val entries = () => {
      if (sourceValue.nonEmpty) {
        Try(JsonDataSource.parseObject(sourceValue)) match {
          case Success(table) if table.nonEmpty =>
            val names = table.head map { h => 
              if (h == "data" && entry != "data") entry else s"$entry.$h"
            }
            val valueList = table.tail
            valueList map { values => 
              ScopedData(DataTable.recordKey, names zip values) 
            }
          case _ => 
            JsonDataSource.parseStringArray(sourceValue) map { value =>
              ScopedData(DataTable.recordKey).set(entry, value)
            }
        }
      } else {
        Nil
      }
    }
    evaluateForEach(entries, entry, parent, step, ctx)
  }

}
