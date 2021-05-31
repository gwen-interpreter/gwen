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

package gwen.core.engine.lambda.composite

import gwen.core.Errors
import gwen.core.engine.EvalContext
import gwen.core.engine.EvalEngine
import gwen.core.model.DataTable
import gwen.core.model.FlatTable
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step

class ForEachTableRecord[T <: EvalContext](doStep: String, engine: EvalEngine[T]) extends ForEach[T](engine) {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    val dataTable = ForEachTableRecord.parseFlatTable(ctx.topScope.getObject(DataTable.tableKey))
    val records = () => {
      dataTable.records.indices.map(idx => dataTable.recordScope(idx))
    }
    evaluateForEach(records, DataTable.recordKey, parent, step, doStep, ctx)
  }

}

object ForEachTableRecord {
  def parseFlatTable(dataTable: Option[Any]): FlatTable = {
    dataTable match {
      case Some(table: FlatTable) => table
      case Some(other) => Errors.dataTableError(s"Cannot use for each on table of type: ${other.getClass.getName}")
      case _ => Errors.dataTableError("Calling step has no data table")
    }
  }
}
