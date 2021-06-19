/*
 * Copyright 2017-2021 Branko Juric, Brady Wood
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

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ScriptSupportTest extends AnyFlatSpec with Matchers with ScriptSupport {

  "JavaScript" should "execute" in {

    val intResult = evaluateJS("2*2")
    intResult should be(4)

    val date = evaluateJS(
      """(function() {
        |  var d = new Date(2017, 6, 8);
        |  return d.getDate()  + '/' + (d.getMonth()) + '/' + d.getFullYear();
        |})();""".stripMargin)
    date should be ("8/6/2017")
  }

  "JavaScript predicate" should "execute" in {

    evaluateJSPredicate("1 == 2") should be(false)
    evaluateJSPredicate("1 == 1") should be(true)

  }

  "JavaScript explicit conversion" should "work" in {

    evaluateJS("'howdy'").asInstanceOf[String] should be ("howdy")
    evaluateJS("5").toString should be ("5")

  }

  "JavaScript date" should "work" in {

    Option(evaluateJS("new Date()").toString) should not be (None)

  }
  
}