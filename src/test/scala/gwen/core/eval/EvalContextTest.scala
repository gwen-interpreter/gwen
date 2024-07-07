/*
 * Copyright 2014-2021 Branko Juric, Brady Wood
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

package gwen.core.eval

import gwen.core.BaseTest
import gwen.core.GwenOptions
import gwen.core.Errors._
import gwen.core.TestModel
import gwen.core.eval.binding.JSBinding
import gwen.core.node.gherkin.table.DataTable
import gwen.core.node.gherkin.table.FlatTable
import gwen.core.node.gherkin.table.TableType
import gwen.core.state.EnvState
import gwen.core.state.ScopedData
import gwen.core.state.StateLevel

import org.scalatest.matchers.should.Matchers

class EvalContextTest extends BaseTest with Matchers with TestModel {
  
  "dry run" should "not call instruction" in {
    val ctx = newCtx(GwenOptions(dryRun = true))
    ctx.perform(sys.error("Execution not expected"))
  }
  
  "non dry run" should "call instruction" in {
    val ctx = newCtx(GwenOptions())
    intercept[Exception] {
      ctx.perform(sys.error("Execution expected"))
    }
  }

  "Data tables and records" should "be accessible until popped" in {

    val table1 = new FlatTable(TableType.horizontal, List(List("1")), List("token"))
    val table2 = new FlatTable(TableType.horizontal, List(List("2")), List("token"))

    val ctx = newCtx
    ctx.topScope.pushObject(DataTable.tableKey, table1)
    ctx.getBoundValue("data[1][token]") should be ("1")
    ctx.topScope.pushObject(DataTable.tableKey, table2)
    ctx.getBoundValue("data[1][token]") should be ("2")
    ctx.topScope.pushObject(DataTable.recordKey, new ScopedData(DataTable.recordKey).set("data[token]", "0"))
    ctx.getBoundValue("data[token]") should be ("0")
    ctx.topScope.popObject(DataTable.recordKey).isDefined should be (true)
    ctx.getBoundValue("data[1][token]") should be ("2")
    ctx.topScope.popObject(DataTable.tableKey).isDefined should be (true)
    ctx.getBoundValue("data[1][token]") should be ("1")
    ctx.topScope.popObject(DataTable.tableKey).isDefined should be (true)
    intercept[UnboundAttributeException] {
      ctx.getBoundValue("data[1][token]")
    }
  }

  "scope with a blank attribute" should """yield blank for getBoundValue call""" in {
    val ctx = newCtx
    ctx.topScope.set("x", "")
    ctx.topScope.set("x", "1")
    ctx.topScope.set("x", "")
    ctx.getBoundValue("x") should be ("")
  }

  "scope with a null attribute" should """yield UnboundAttributeException for getBoundValue call""" in {
    val ctx = newCtx
    ctx.topScope.set("x", null)
    intercept[UnboundAttributeException] {
      ctx.getBoundValue("x")
    }
  }

  "scope clear on missing attribute" should """do nothing""" in {
    val ctx = newCtx
    ctx.topScope.clear("x")
    ctx.topScope.getOpt("x") should be (None)
  }

  "scope with a null attribute overriding non null attribute" should """yield UnboundAttributeException for getBoundValue call""" in {
    val ctx = newCtx
    ctx.topScope.set("x", "1")
    ctx.getBoundValue("x") should be ("1")
    ctx.topScope.set("x", null)
    intercept[UnboundAttributeException] {
      ctx.getBoundValue("x")
    }
  }

  "scope with clear on attribute overriding non null attribute" should """yield UnboundAttributeException for getBoundValue call""" in {
    val ctx = newCtx
    ctx.topScope.set("x", "1")
    ctx.getBoundValue("x") should be ("1")
    ctx.topScope.clear("x")
    intercept[UnboundAttributeException] {
      ctx.getBoundValue("x")
    }
  }

  """Property that's bound to a good javascript""" should "should pass interpolation" in {
    val ctx = newCtx
    JSBinding.bind("length", "'Good length function'.length()", false, ctx)
    ctx.interpolate("""${length} chars""") should be ("20 chars")
  }

  """Property that's bound to a bad javascript""" should "should fail interpolation" in {
    val ctx = newCtx
    JSBinding.bind("length", "'Bad length function'.len()", false, ctx)
    intercept[FunctionException] {
      ctx.interpolate("""${length} chars""")
    }
  }

  "top binding override in local scope" should "be visible in flash scope" in {
    val ctx = newCtx
    ctx.topScope.set("e", "1")
    ctx.getBoundValue("e") should be ("1")
    ctx.scopes.addScope("E")
    ctx.getBoundValue("e") should be ("1")
    JSBinding.bind("e", "2 + 1", false, ctx)
    ctx.topScope.set("e", ctx.getBoundValue("e"))
    ctx.topScope.set("ee", "3")
    ctx.getBoundValue("ee") should be ("3")
    ctx.topScope.set("e", "2")                // will create a copy in local flash scope
    ctx.getBoundValue("e") should be ("2")    // will source value from local flash scope copy
    JSBinding.bind("e", "2 + 2", false, ctx)
    ctx.getBoundValue("e") should be ("4")
  }
  
  private def newCtx: EvalContext = newCtx(GwenOptions())
  
  private def newCtx(options: GwenOptions): EvalContext = {
    new EvalContext(options, EnvState()) {
      override def close(): Unit = {
        super.reset(StateLevel.feature)
      }
    }
  }

}
