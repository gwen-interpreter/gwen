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

package gwen.core.engine

import gwen.core.BaseTest
import gwen.core.GwenOptions
import gwen.core.Errors._
import gwen.core.TestModel
import gwen.core.model._
import gwen.core.model.state.EnvState
import gwen.core.model.state.ScopedData

import org.scalatest.Matchers

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

    val table1 = new FlatTable(List(List("1")), List("token"))
    val table2 = new FlatTable(List(List("2")), List("token"))

    val ctx = newCtx
    ctx.topScope.pushObject(DataTable.tableKey, table1)
    ctx.getBoundReferenceValue("data[1][token]") should be ("1")
    ctx.topScope.pushObject(DataTable.tableKey, table2)
    ctx.getBoundReferenceValue("data[1][token]") should be ("2")
    ctx.topScope.pushObject(DataTable.recordKey, new ScopedData(DataTable.recordKey).set("data[token]", "0"))
    ctx.getBoundReferenceValue("data[token]") should be ("0")
    ctx.topScope.popObject(DataTable.recordKey).isDefined should be (true)
    ctx.getBoundReferenceValue("data[1][token]") should be ("2")
    ctx.topScope.popObject(DataTable.tableKey).isDefined should be (true)
    ctx.getBoundReferenceValue("data[1][token]") should be ("1")
    ctx.topScope.popObject(DataTable.tableKey).isDefined should be (true)
    intercept[UnboundAttributeException] {
      ctx.getBoundReferenceValue("data[1][token]")
    }
  }

  "scope with a blank attribute" should """yield blank for getBoundReferenceValue call""" in {
    val ctx = newCtx
    ctx.topScope.set("x", "")
    ctx.topScope.set("x", "1")
    ctx.topScope.set("x", "")
    ctx.getBoundReferenceValue("x") should be ("")
  }

  "scope with a null attribute" should """yield UnboundAttributeException for getBoundReferenceValue call""" in {
    val ctx = newCtx
    ctx.topScope.set("x", null)
    intercept[UnboundAttributeException] {
      ctx.getBoundReferenceValue("x")
    }
  }

  "scope with a null attribute overriding non null attribute" should """yield UnboundAttributeException for getBoundReferenceValue call""" in {
    val ctx = newCtx
    ctx.topScope.set("x", "1")
    ctx.getBoundReferenceValue("x") should be ("1")
    ctx.topScope.set("x", null)
    intercept[UnboundAttributeException] {
      ctx.getBoundReferenceValue("x")
    }
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
