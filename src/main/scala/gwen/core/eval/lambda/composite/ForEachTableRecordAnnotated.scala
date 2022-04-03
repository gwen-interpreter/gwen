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

package gwen.core.eval.lambda.composite

import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Annotations
import gwen.core.node.gherkin.Step
import gwen.core.node.gherkin.Scenario
import gwen.core.node.gherkin.table.DataTable
import gwen.core.node.gherkin.table.FlatTable

class ForEachTableRecordAnnotated[T <: EvalContext](stepDef: Scenario, step: Step, dataTable: FlatTable,  engine: EvalEngine[T]) extends ForEach[T](engine, stepDef.name) {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    ctx.topScope.pushObject(DataTable.tableKey, dataTable)
    val doStepDef = stepDef.copy(
      withTags = stepDef.tags filter { tag => 
        tag.name != Annotations.ForEach.toString &&
        !tag.name.startsWith(Annotations.DataTable.toString)
      }
    )
    ctx.removeStepDef(stepDef.name)
    ctx.addStepDef(doStepDef)
    try {
      val records = () => {
        dataTable.records.indices.map(idx => dataTable.recordScope(idx))
      }
      evaluateForEach(records, DataTable.recordKey, parent, step, ctx)
    } finally {
      ctx.removeStepDef(doStepDef.name)
      ctx.addStepDef(stepDef)
      ctx.topScope.popObject(DataTable.tableKey)
    }
  }

}
