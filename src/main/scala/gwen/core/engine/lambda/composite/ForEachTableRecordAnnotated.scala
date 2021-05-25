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

import gwen.core.engine.EvalContext
import gwen.core.engine.EvalEngine
import gwen.core.model.DataTable
import gwen.core.model.FlatTable
import gwen.core.model.ReservedTags
import gwen.core.model.Identifiable
import gwen.core.model.node.Step
import gwen.core.model.node.Scenario

class ForEachTableRecordAnnotated[T <: EvalContext](stepDef: Scenario, step: Step, dataTable: FlatTable,  engine: EvalEngine[T]) extends ForEach[T](engine) {

  override def apply(parent: Identifiable, step: Step, ctx: T): Step = {
    ctx.topScope.pushObject(DataTable.tableKey, dataTable)
    val doStepDef = stepDef.copy(
      withTags = stepDef.tags filter { tag => 
        tag.name != ReservedTags.ForEach.toString &&
        !tag.name.startsWith(ReservedTags.DataTable.toString)
      }
    )
    ctx.removeStepDef(stepDef.name)
    ctx.addStepDef(doStepDef)
    try {
      val records = () => {
        dataTable.records.indices.map(idx => dataTable.recordScope(idx))
      }
      evaluateForEach(records, DataTable.recordKey, parent, step, doStepDef.name, ctx)
    } finally {
      ctx.removeStepDef(doStepDef.name)
      ctx.addStepDef(stepDef)
      ctx.topScope.popObject(DataTable.tableKey)
    }
  }

}
