/*
 * Copyright 2017-2022 Branko Juric, Brady Wood
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

package gwen.core.eval.support

import gwen.core.BaseTest
import gwen.core.GwenOptions
import gwen.core.eval.EvalContext
import gwen.core.state.EnvState

import org.scalatest.matchers.should.Matchers

class JavaScriptSupportTest extends BaseTest with Matchers {

  private val jsSupport = new EvalContext(GwenOptions(), EnvState()).asInstanceOf[JavaScriptSupport[EvalContext]]

  "JavaScript function" should "execute" in {

    val intResult = jsSupport.evaluateJS("2*2")
    intResult should be(4)

    val date = jsSupport.evaluateJS(
      """(function() {
        |  var d = new Date(2017, 6, 8);
        |  return d.getDate()  + '/' + (d.getMonth()) + '/' + d.getFullYear();
        |})();""".stripMargin)
    date should be ("8/6/2017")
  }

  "JavaScript arrow function" should "execute" in {

    val intResult = jsSupport.evaluateJS("() => 2*2")
    intResult should be(4)

    val date = jsSupport.evaluateJS(
      """ () => {
         |  var d = new Date(2017, 6, 8);
         |  return d.getDate()  + '/' + (d.getMonth()) + '/' + d.getFullYear();
         |}""".stripMargin)
    date should be ("8/6/2017")
  }

  "JavaScript explicit conversion" should "work" in {

    jsSupport.evaluateJS("'howdy'").asInstanceOf[String] should be ("howdy")
    jsSupport.evaluateJS("5").toString should be ("5")

  }

  "JavaScript date" should "work" in {

    Option(jsSupport.evaluateJS("new Date()").toString) should not be (None)

  }
  
}