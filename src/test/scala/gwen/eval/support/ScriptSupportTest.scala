/*
 * Copyright 2017 Branko Juric, Brady Wood
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

package gwen.eval.support

import org.scalatest.{FlatSpec, Matchers}

class ScriptSupportTest extends FlatSpec with Matchers with ScriptSupport {

  "JavaScript" should "coerce result" in {

    val intResult: Int = evaluateJavaScript("2*2")
    intResult should be(4)

    val booleanResult: Boolean = evaluateJavaScript("1 == 2")
    booleanResult should be(false)

    val date: String = evaluateJavaScript("var d = new Date(2017, 6, 8); d.getDate()  + '/' + (d.getMonth()) + '/' + d.getFullYear();")
    date should be ("8/6/2017")
  }

  "JavaScript explicit conversion" should "work" in {

    evaluateJavaScript("'howdy'").asInstanceOf[String] should be ("howdy")
    evaluateJavaScript("5").toString should be ("5")

  }

  "JavaScript date" should "work" in {

    Option(evaluateJavaScript("new Date()").toString) should not be (None)

  }
  
}