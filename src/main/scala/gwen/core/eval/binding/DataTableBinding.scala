/*
 * Copyright 2021-2024 Branko Juric, Brady Wood
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

package gwen.core.eval.binding

import gwen.core.Errors
import gwen.core.Settings
import gwen.core.eval.EvalContext
import gwen.core.node.gherkin.table.DataTable

class DataTableBinding[T <: EvalContext](name: String, ctx: T) extends Binding[T, String](name, ctx) {

  override def resolve(): String = {
    ctx.topScope.getObject(DataTable.tableKey) match {
      case Some(table: DataTable) => 
        table.tableScope.getOpt(name) getOrElse {
          ctx.topScope.getOpt(name) getOrElse {
            Settings.getOpt(name) getOrElse {
              Errors.unboundReferenceError(name)
            }
          }
        }
      case _ => Errors.unboundReferenceError(name)
    }
  }

  override def toString: String = name

}
