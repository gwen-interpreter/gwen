/*
 * Copyright 2015-2021 Branko Juric, Brady Wood
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

import gwen.core.Settings

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import gwen.core.Errors

class InterpolationSupportTest extends FlatSpec with Matchers with InterpolationSupport {

  """interpolate using property syntax: prefix "${property}"""" should "resolve" in {
    interpolateString("""hello "${property}"""") { _ => "you" } should be ("""hello "you"""")
  }
  
  """interpolate using property syntax: prefix ${property}""" should "resolve" in {
    interpolateString("""hello ${property}""") { _ => "you" } should be ("""hello you""")
  }
  
  """interpolate using property syntax: "${property}" suffix""" should "resolve" in {
    interpolateString(""""${property}" you""") { _ => "hello" } should be (""""hello" you""")
  }
  
  """interpolate using property syntax: ${property} "suffix"""" should "resolve" in {
    interpolateString("""${property} "you"""") { _ => "hello" } should be ("""hello "you"""")
  }
  
  """interpolate using property syntax: prefix ${property} suffix""" should "resolve" in {
    interpolateString("""hello ${property} good thing""") { _ => "you" } should be ("""hello you good thing""")
  }
  
  """interpolate nested using property syntax: ${property1-${property0}}"""" should "resolve" in {
    interpolateString("""Hey you ${property-${id}} thing!""") {
      case "id" => "0"
      case "property-0" => "good"
      case _ => "undefined"
    } should be ("""Hey you good thing!""")
  }
  
  """interpolate adjacent values using property syntax: ${property-0} ${property-1}"""" should "resolve" in {
    interpolateString("""Hey you ${property-0} ${property-1} thing!""") {
      case "id" => "0"
      case "property-0" => "really"
      case "property-1" => "good"
      case _ => "undefined"
    } should be ("""Hey you really good thing!""")
  }
  
  """interpolate adjacent values using property syntax (no space): ${property-0}${property-1}"""" should "resolve" in {
    interpolateString("""Hey you ${property-0}${property-1} thing!""") {
      case "id" => "0"
      case "property-0" => "go"
      case "property-1" => "od"
      case _ => "undefined"
    } should be ("""Hey you good thing!""")
  }
  
  """interpolate using stepdef param syntax: prefix "$<param>"""" should "resolve" in {
    interpolateString("""hello "$<param>"""") { _ => "you" } should be ("""hello "you"""")
  }
  
  """interpolate using stepdef param syntax: prefix $<param>""" should "resolve" in {
    interpolateString("""hello $<param>""") { _ => "you" } should be ("""hello you""")
  }
  
  """interpolate using stepdef param syntax: "$<param>" suffix""" should "resolve" in {
    interpolateString(""""$<param>" you""") { _ => "hello" } should be (""""hello" you""")
  }
  
  """interpolate using stepdef param syntax: $<param> "suffix"""" should "resolve" in {
    interpolateString("""$<param> "you"""") { _ => "hello" } should be ("""hello "you"""")
  }
  
  """interpolate using stepdef param syntax: prefix $<param> suffix""" should "resolve" in {
    interpolateString("""hello $<param> good thing""") { _ => "you" } should be ("""hello you good thing""")
  }
  
  """interpolate nested using stepdef param syntax: $<param1-$<param0>>"""" should "resolve" in {
    interpolateString("""Hey you $<param-$<id>> thing!""") {
      case "<id>" => "0"
      case "<param-0>" => "good"
      case _ => "undefined"
    } should be ("""Hey you good thing!""")
  }
  
  """interpolate stepdef with adjacent params: $<param-0> $<param-1>"""" should "resolve" in {
    interpolateString("""Hey you $<param-0> $<param-1> thing!""") {
      case "<param-0>" => "really"
      case "<param-1>" => "good"
      case _ => "undefined"
    } should be ("""Hey you really good thing!""")
  }
  
  """interpolate stepdef with adjacent params (no space): $<param-0>$<param-1>"""" should "resolve" in {
    interpolateString("""Hey you $<param-0>$<param-1> thing!""") {
      case "<param-0>" => "go"
      case "<param-1>" => "od"
      case _ => "undefined"
    } should be ("""Hey you good thing!""")
  }
  
  """interpolating stepdef in dry run mode: $<param-0>$<param-1>"""" should "decorate parameters" in {
    interpolateString("""Hey you $<param-0>$<param-1> thing!""") {
      case "<param-0>" => "$<param-0>"
      case "<param-1>" => "$<param-1>"
      case _ => "undefined"
    } should be ("""Hey you $[param:param-0]$[param:param-1] thing!""")
  }
  
  """interpolate using concatentation syntax: prefix "" + binding + " suffix""" should "resolve" in {
    interpolateString("""hello "" + binding + " good thing"""") { _ => "you" } should be ("""hello "you good thing"""")
  }
 
  """interpolate using concatentation syntax: "prefix" + binding""" should "resolve" in {
    interpolateString("""hello "" + binding""") { _ => "you" } should be ("""hello "you"""")
  }
  
  """embedded + literal in string""" should "not be treated as a concatenation operator" in {
    interpolateString("""I enter "+6149265587" in the phone field""") { _ => throw new Exception("should not throw this") } should be ("""I enter "+6149265587" in the phone field""")
  }

  """interpolate env var using property syntax: prefix "${property}"""" should "resolve" in {
    interpolateString("""home "${env.HOME}"""") { _ => Settings.getOpt("env.HOME"). getOrElse("") } should be (s"""home "${sys.env("HOME")}"""")
  }
  
  """interpolate env var using property syntax: prefix ${property}""" should "resolve" in {
    interpolateString("""home ${env.HOME}""") { _ => Settings.getOpt("env.HOME"). getOrElse("") } should be (s"""home ${sys.env("HOME")}""")
  }
  
  """interpolate env var using property syntax: "${property}" suffix""" should "resolve" in {
    interpolateString(""""${env.HOME}" home""") { _ => Settings.getOpt("env.HOME"). getOrElse("") } should be (s""""${sys.env("HOME")}" home""")
  }
  
  """interpolate env var using property syntax: ${property} "suffix"""" should "resolve" in {
    interpolateString("""${property} "you"""") { _ => "hello" } should be ("""hello "you"""")
    interpolateString("""${env.HOME} home""") { _ => Settings.getOpt("env.HOME"). getOrElse("") } should be (s"""${sys.env("HOME")} home""")
  }
  
  """interpolate env var using property syntax: prefix ${property} suffix""" should "resolve" in {
    interpolateString("""hello ${property} good thing""") { _ => "you" } should be ("""hello you good thing""")
    interpolateString("""home ${env.HOME} found""") { _ => Settings.getOpt("env.HOME"). getOrElse("") } should be (s"""home ${sys.env("HOME")} found""")
  }
  
  """interpolate nested env var using property syntax: ${property1-${property0}}"""" should "resolve" in {
    interpolateString("""Hey you ${env.var-${env.id}} thing!""") {
      case "env.id" => "0"
      case "env.var-0" => "good"
      case _ => "undefined"
    } should be ("""Hey you good thing!""")
  }
  
  """interpolate adjacent env var values using property syntax: ${property-0} ${property-1}"""" should "resolve" in {
    interpolateString("""Hey you ${env.var0} ${env.var1} thing!""") {
      case "env.var0" => "really"
      case "env.var1" => "good"
      case _ => "undefined"
    } should be ("""Hey you really good thing!""")
  }
  
  """interpolate adjacent env var values using property syntax (no space): ${property-0}${property-1}"""" should "resolve" in {
    interpolateString("""Hey you ${env.var0}${env.var1} thing!""") {
      case "env.var0" => "go"
      case "env.var1" => "od"
      case _ => "undefined"
    } should be ("""Hey you good thing!""")
  }

  """multi line string with properties""" should "resolve" in {
    val source =
      """hello
        |${property}""".stripMargin
    val target =
      """hello
        |you""".stripMargin
    interpolateString(source) { _ => "you" } should be(target)
  }

  """multi line string with env vars""" should "resolve" in {
    val source =
      """hello
        |${env.var}""".stripMargin
    val target =
      """hello
        |you""".stripMargin
    interpolateString(source) { _ => "you" } should be(target)
  }

  """Nested parameter in property: ${property-$<param>}"""" should "resolve" in {
    interpolateString("""Hey you ${property-$<id>} thing!""") {
      case "<id>" => "0"
      case "property-0" => "good"
      case x => s"undefined($x)"
    } should be ("""Hey you good thing!""")
  }

  """Nested parameter in env var: ${env.var_$<param>}"""" should "resolve" in {
    interpolateString("""Hey you ${env.var_$<id>} thing!""") {
      case "<id>" => "0"
      case "env.var_0" => "good"
      case x => s"undefined($x)"
    } should be ("""Hey you good thing!""")
  }

  """Nested property in parameter: $<property-${param}>"""" should "resolve" in {
    interpolateString("""Hey you $<property-${id}> thing!""") {
      case "id" => "0"
      case "<property-0>" => "good"
      case x => s"undefined($x)"
    } should be ("""Hey you good thing!""")
  }

  """Nested env var in parameter: $<property-${env.var}>"""" should "resolve" in {
    interpolateString("""Hey you $<property-${env.var}> thing!""") {
      case "env.var" => "0"
      case "<property-0>" => "good"
      case x => s"undefined($x)"
    } should be ("""Hey you good thing!""")
  }

  """Interpolation of Params""" should "resolve 1 available param" in {
    interpolateParams("""Hey you ${env.var0} $<param> thing!""") {
      case "<param>" => "good"
      case x => Errors.unboundAttributeError(x, "local")
    } should be ("""Hey you ${env.var0} good thing!""")
  }

  """Interpolation of Params""" should "resolve 2 available params" in {
    interpolateParams("""Hey you $<param1> ${env.var0} thing $<param2>!""") {
      case "<param1>" => "good"
      case "<param2>" => "you"
      case x => Errors.unboundAttributeError(x, "local")
    } should be ("""Hey you good ${env.var0} thing you!""")
  }

  """Interpolation of Params""" should "resolve 2 available params and skip missing param" in {
    interpolateParams("""Hey you $<param1> $<param2> thing $<param3>!""") {
      case "<param1>" => "good"
      case "<param3>" => "you"
      case x => Errors.unboundAttributeError(x, "local")
    } should be ("""Hey you good $<param2> thing you!""")
  }

  """Interpolation of Params""" should "resolve 2 available params and skip composite param" in {
    interpolateParams("""Hey you $<param1> $<${env.var0}> thing $<param2>!""") {
      case "<param1>" => "good"
      case "<param2>" => "you"
      case x => Errors.unboundAttributeError(x, "local")
    } should be ("""Hey you good $<${env.var0}> thing you!""")
  }

}