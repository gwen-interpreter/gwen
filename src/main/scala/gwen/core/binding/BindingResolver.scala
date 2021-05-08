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

package gwen.core.eval.binding

import gwen.core.Errors
import gwen.core.Settings
import gwen.core.eval.EvalContext
import gwen.core.model.DataTable
import gwen.core.model.state.ScopedData

/** 
 * Resolves all bindings.
 */
class BindingResolver[T <: EvalContext](ctx: T) {

  /**
    * Find the binding with the given name and resolves it.
    *
    * @param name the binding name
    * @return the resolved value
    */
  def resolve(name: String): String = getBinding(name).resolve()

  /**
    * Finds and returns a given binding.
    *
    * @param name the bindnng name
    * @return a binding
    */
  def getBinding(name: String): Binding[T, String] = ctx.withEnv { env =>
    val visibleScopes = env.scopes.visible
    val attScopes = visibleScopes.filterAtts{case (n, _) => n.startsWith(name)}
    attScopes.findEntry { case (n, _) => 
      n.matches(s"""$name(/(text|javascript|xpath.+|regex.+|json path.+|sysproc|file|sql.+))?""")
    } map { case (n, _) =>
      if (n == TextBinding.key(name)) new TextBinding(name, ctx)
      else if (n == JavaScriptBinding.key(name)) new  JavaScriptBinding(name, ctx)
      else if (n.startsWith(XPathBinding.baseKey(name))) new XPathBinding(name, ctx)
      else if (n.startsWith(RegexBinding.baseKey(name))) new RegexBinding(name, ctx)
      else if (n.startsWith(JsonPathBinding.baseKey(name))) new JsonPathBinding(name, ctx)
      else if (n == SysprocBinding.key(name)) new SysprocBinding(name, ctx)
      else if (n == FileBinding.key(name)) new FileBinding(name, ctx)
      else if (n.startsWith(SQLBinding.baseKey(name))) new SQLBinding(name, ctx)
      else new  SimpleBinding(name, ctx)
    } getOrElse {
      (env.topScope.getObject(DataTable.recordKey) match {
        case Some(record: ScopedData) => Some(new DataRecordBinding(name, ctx))
        case _ => env.topScope.getObject(DataTable.tableKey) match {
          case Some(table: DataTable) => Some(new DataTableBinding(name, ctx))
          case _ => None
        }
      }).getOrElse {
        env.scopes.getOpt(name) map { _ => 
          new SimpleBinding(name, ctx)
        } getOrElse {
          Settings.getOpt(name) map { _ => 
            new SettingsBinding(name, ctx)
          } getOrElse {
            Errors.unboundAttributeError(name)
          }
        }
      }
    }
  }

}

 