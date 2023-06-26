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

package gwen.core

import gwen.core.BaseTest
import gwen.core.Settings

import org.scalatest.matchers.should.Matchers
import gwen.core.Errors.UnboundAttributeException

class InterpolatorTest extends BaseTest with Matchers {

  """interpolate using property syntax: prefix "${property}"""" should "resolve" in {
    interpolate("""hello "${property}"""") { _ => Some("you") } should be ("""hello "you"""")
  }
  
  """interpolate using property syntax: prefix ${property}""" should "resolve" in {
    interpolate("""hello ${property}""") { _ => Some("you") } should be ("""hello you""")
  }
  
  """interpolate using property syntax: "${property}" suffix""" should "resolve" in {
    interpolate(""""${property}" you""") { _ => Some("hello") } should be (""""hello" you""")
  }
  
  """interpolate using property syntax: ${property} "suffix"""" should "resolve" in {
    interpolate("""${property} "you"""") { _ => Some("hello") } should be ("""hello "you"""")
  }
  
  """interpolate using property syntax: prefix ${property} suffix""" should "resolve" in {
    interpolate("""hello ${property} good thing""") { _ => Some("you") } should be ("""hello you good thing""")
  }
  
  """interpolate nested using property syntax: ${property1-${property0}}"""" should "resolve" in {
    interpolate("""Go you ${property-${id}} thing!""") {
      case "id" => Some("0")
      case "property-0" => Some("good")
      case _ => None
    } should be ("""Go you good thing!""")
  }
  
  """interpolate adjacent values using property syntax: ${property-0} ${property-1}"""" should "resolve" in {
    interpolate("""Go you ${property-0} ${property-1} thing!""") {
      case "property-0" => Some("really")
      case "property-1" => Some("good")
      case _ => None
    } should be ("""Go you really good thing!""")
  }
  
  """interpolate adjacent values using property syntax (no space): ${property-0}${property-1}"""" should "resolve" in {
    interpolate("""Go you ${property-0}${property-1} thing!""") {
      case "property-0" => Some("go")
      case "property-1" => Some("od")
      case _ => None
    } should be ("""Go you good thing!""")
  }
  
  """interpolate using stepdef param syntax: prefix "$<param>"""" should "resolve" in {
    interpolate("""hello "$<param>"""") { _ => Some("you") } should be ("""hello "you"""")
  }
  
  """interpolate using stepdef param syntax: prefix $<param>""" should "resolve" in {
    interpolate("""hello $<param>""") { _ => Some("you") } should be ("""hello you""")
  }
  
  """interpolate using stepdef param syntax: "$<param>" suffix""" should "resolve" in {
    interpolate(""""$<param>" you""") { _ => Some("hello") } should be (""""hello" you""")
  }
  
  """interpolate using stepdef param syntax: $<param> "suffix"""" should "resolve" in {
    interpolate("""$<param> "you"""") { _ => Some("hello") } should be ("""hello "you"""")
  }
  
  """interpolate using stepdef param syntax: prefix $<param> suffix""" should "resolve" in {
    interpolate("""hello $<param> good thing""") { _ => Some("you") } should be ("""hello you good thing""")
  }
  
  """interpolate nested using stepdef param syntax: $<param1-$<param0>>"""" should "resolve" in {
    interpolate("""Go you $<param-$<id>> thing!""") {
      case "<id>" => Some("0")
      case "<param-0>" => Some("good")
      case _ => None
    } should be ("""Go you good thing!""")
  }
  
  """interpolate stepdef with adjacent params: $<param-0> $<param-1>"""" should "resolve" in {
    interpolate("""Go you $<param-0> $<param-1> thing!""") {
      case "<param-0>" => Some("really")
      case "<param-1>" => Some("good")
      case _ => None
    } should be ("""Go you really good thing!""")
  }
  
  """interpolate stepdef with adjacent params (no space): $<param-0>$<param-1>"""" should "resolve" in {
    interpolate("""Go you $<param-0>$<param-1> thing!""") {
      case "<param-0>" => Some("go")
      case "<param-1>" => Some("od")
      case _ => None
    } should be ("""Go you good thing!""")
  }
  
  """interpolating stepdef in lenient mode: $<param-0>$<param-1>"""" should "preserve parameter placeholders" in {
    interpolate("""Go you $<param-0>$<param-1> thing!""", lenient = true) {
      case _ => None
    } should be ("""Go you $<param-0>$<param-1> thing!""")
  }
  
  """embedded + literal in string""" should "not be treated as a concatenation operator" in {
    interpolate("""I enter "+6149265587" in the phone field""") { _ => throw new Exception("should not throw this") } should be ("""I enter "+6149265587" in the phone field""")
  }

  """interpolate env var using property syntax: prefix "${property}"""" should "resolve" in {
    interpolate("""home "${env.HOME}"""") { _ => Settings.getOpt("env.HOME") } should be (s"""home "${sys.env("HOME")}"""")
  }
  
  """interpolate env var using property syntax: prefix ${property}""" should "resolve" in {
    interpolate("""home ${env.HOME}""") { _ => Settings.getOpt("env.HOME") } should be (s"""home ${sys.env("HOME")}""")
  }
  
  """interpolate env var using property syntax: "${property}" suffix""" should "resolve" in {
    interpolate(""""${env.HOME}" home""") { _ => Settings.getOpt("env.HOME") } should be (s""""${sys.env("HOME")}" home""")
  }
  
  """interpolate env var using property syntax: ${property} "suffix"""" should "resolve" in {
    interpolate("""${property} "you"""") { _ => Some("hello") } should be ("""hello "you"""")
    interpolate("""${env.HOME} home""") { _ => Settings.getOpt("env.HOME") } should be (s"""${sys.env("HOME")} home""")
  }
  
  """interpolate env var using property syntax: prefix ${property} suffix""" should "resolve" in {
    interpolate("""hello ${property} good thing""") { _ => Some("you") } should be ("""hello you good thing""")
    interpolate("""home ${env.HOME} found""") { _ => Settings.getOpt("env.HOME") } should be (s"""home ${sys.env("HOME")} found""")
  }
  
  """interpolate nested env var using property syntax: ${property1-${property0}}"""" should "resolve" in {
    interpolate("""Go you ${env.var-${env.id}} thing!""") {
      case "env.id" => Some("0")
      case "env.var-0" => Some("good")
      case _ => None
    } should be ("""Go you good thing!""")
  }
  
  """interpolate adjacent env var values using property syntax: ${property-0} ${property-1}"""" should "resolve" in {
    interpolate("""Go you ${env.var0} ${env.var1} thing!""") {
      case "env.var0" => Some("really")
      case "env.var1" => Some("good")
      case _ => None
    } should be ("""Go you really good thing!""")
  }
  
  """interpolate adjacent env var values using property syntax (no space): ${property-0}${property-1}"""" should "resolve" in {
    interpolate("""Go you ${env.var0}${env.var1} thing!""") {
      case "env.var0" => Some("go")
      case "env.var1" => Some("od")
      case _ => None
    } should be ("""Go you good thing!""")
  }

  """multi line string with properties""" should "resolve" in {
    val source =
      """hello
        |${property}""".stripMargin
    val target =
      """hello
        |you""".stripMargin
    interpolate(source) { _ => Some("you") } should be(target)
  }

  """multi line string with env vars""" should "resolve" in {
    val source =
      """hello
        |${env.var}""".stripMargin
    val target =
      """hello
        |you""".stripMargin
    interpolate(source) { _ => Some("you") } should be(target)
  }

  """Nested parameter in property: ${property-$<param>}"""" should "resolve" in {
    interpolate("""Go you ${property-$<id>} thing!""") {
      case "<id>" => Some("0")
      case "property-0" => Some("good")
      case _ => None
    } should be ("""Go you good thing!""")
  }

  """Nested parameter in env var: ${env.var_$<param>}"""" should "resolve" in {
    interpolate("""Go you ${env.var_$<id>} thing!""") {
      case "<id>" => Some("0")
      case "env.var_0" => Some("good")
      case _ => None
    } should be ("""Go you good thing!""")
  }

  """Nested property in parameter: $<param-${property}>"""" should "resolve" in {
    interpolate("""Go you $<param-${id}> thing!""") {
      case "id" => Some("0")
      case "<param-0>" => Some("good")
      case _ => None
    } should be ("""Go you good thing!""")
  }

  """Nested env var in parameter: $<param-${env.var}>"""" should "resolve" in {
    interpolate("""Go you $<param-${env.var}> thing!""") {
      case "env.var" => Some("0")
      case "<param-0>" => Some("good")
      case _ => None
    } should be ("""Go you good thing!""")
  }

  """Interpolation of Params""" should "resolve 1 available param" in {
    interpolateParams("""Go you ${env.var0} $<param> thing!""") {
      case "<param>" => Some("good")
      case _ => None
    } should be ("""Go you ${env.var0} good thing!""")
  }

  """Interpolation of Params""" should "resolve 2 available params" in {
    interpolateParams("""Go you $<param1> ${env.var0} thing $<param2>!""") {
      case "<param1>" => Some("good")
      case "<param2>" => Some("you")
      case _ => None
    } should be ("""Go you good ${env.var0} thing you!""")
  }

  """Interpolation of Params in lemient mode""" should "resolve 2 available params and skip missing param" in {
    interpolateParams("""Go you $<param1> $<param2> thing $<param3>!""", lenient = true) {
      case "<param1>" => Some("good")
      case "<param3>" => Some("you")
      case _ => None
    } should be ("""Go you good $<param2> thing you!""")
  }

  """Interpolation of Params in lenient mode""" should "resolve 2 available params and skip composite param" in {
    interpolateParams("""Go you $<param1> $<${env.var0}> thing $<param2>!""", lenient = true) {
      case "<param1>" => Some("good")
      case "<param2>" => Some("you")
      case _ => None
    } should be ("""Go you good $<${env.var0}> thing you!""")
  }

  """Same named property and parameter: $<name> ${name}"""" should "resolve" in {
    interpolate("""Go you $<name> ${name}!""") {
      case "name" => Some("thing")
      case "<name>" => Some("good")
      case _ => None
    } should be ("""Go you good thing!""")
  }

  """Same named parameter and property: ${name} $<name>"""" should "resolve" in {
    interpolate("""Go you ${name} $<name>!""") {
      case "name" => Some("good")
      case "<name>" => Some("thing")
      case _ => None
    } should be ("""Go you good thing!""")
  }

  """Nested parameter that resolves to same named property: $<${name}>"""" should "resolve" in {
    interpolate("""Go you $<${name}>!""") {
      case "name" => Some("name")
      case "<name>" => Some("good thing")
      case _ => None
    } should be ("""Go you good thing!""")
  }

  """Nested property that resolves to same named parameter: ${$<name>}"""" should "resolve" in {
    interpolate("""Go you ${$<name>}!""") {
      case "name" => Some("good thing")
      case "<name>" => Some("name")
      case _ => None
    } should be ("""Go you good thing!""")
  }

  """Property that starts with csv.record.""" should "not resolve in default interpolator" in {
    interpolate("""Hello ${csv.record.Name}!""") {
      case _ => None
    } should be ("""Hello ${csv.record.Name}!""")
  }

  """Property that starts with csv.record.""" should "not resolve in lenient interpolator" in {
    interpolate("""Hello ${csv.record.Name}!""", lenient = true) {
      case _ => None
    } should be ("""Hello ${csv.record.Name}!""")
  }

  """Property that starts with csv.record.""" should "resolve in strict interpolator" in {
    interpolateStrict("""Hello ${csv.record.Name}!""") {
      case "csv.record.Name" => Some("Gwen")
      case _ => None
    } should be ("""Hello Gwen!""")
  }

  """Property that starts with csv.record.""" should "fail in strict interpolator if not bound at all" in {
    intercept[UnboundAttributeException] {
      interpolateStrict("""Hello ${csv.record.Name}!""") {
        case _ => None
      }
    }
  }

  """Property that starts with csv.record.""" should "fail in strict interpolator if not bound" in {
    intercept[UnboundAttributeException] {
      interpolateStrict("""Hello ${csv.record.Name}!""") {
        case "csv.record.Namex" => Some("Gwen")
        case _ => None
      }
    }
  }

  """Property that starts with json.record.""" should "not resolve in default interpolator" in {
    interpolate("""Hello ${json.record.Name}!""") {
      case _ => None
    } should be ("""Hello ${json.record.Name}!""")
  }

  """Property that starts with json.record.""" should "not resolve in lenient interpolator" in {
    interpolate("""Hello ${json.record.Name}!""", lenient = true) {
      case _ => None
    } should be ("""Hello ${json.record.Name}!""")
  }

  """Property that starts with json.record.""" should "resolve in strict interpolator" in {
    interpolateStrict("""Hello ${json.record.Name}!""") {
      case "json.record.Name" => Some("Gwen")
      case _ => None
    } should be ("""Hello Gwen!""")
  }

  """Property that starts with json.record.""" should "fail in strict interpolator if not bound at all" in {
    intercept[UnboundAttributeException] {
      interpolateStrict("""Hello ${json.record.Name}!""") {
        case _ => None
      }
    }
  }

  """Property that starts with json.record.""" should "fail in strict interpolator if not bound" in {
    intercept[UnboundAttributeException] {
      interpolateStrict("""Hello ${json.record.Name}!""") {
        case "json.record.Namex" => Some("Gwen")
        case _ => None
      }
    }
  }

  """Property that starts with data.record.""" should "not resolve in default interpolator" in {
    interpolate("""Hello ${data.record.Name}!""") {
      case _ => None
    } should be ("""Hello ${data.record.Name}!""")
  }

  """Property that starts with data.record.""" should "not resolve in lenient interpolator" in {
    interpolate("""Hello ${data.record.Name}!""", lenient = true) {
      case _ => None
    } should be ("""Hello ${data.record.Name}!""")
  }

  """Property that starts with data.record.""" should "resolve in strict interpolator" in {
    interpolateStrict("""Hello ${data.record.Name}!""") {
      case "data.record.Name" => Some("Gwen")
      case _ => None
    } should be ("""Hello Gwen!""")
  }

  """Property that starts with data.record.""" should "fail in strict interpolator if not bound at all" in {
    intercept[UnboundAttributeException] {
      interpolateStrict("""Hello ${data.record.Name}!""") {
        case _ => None
      }
    }
  }

  """Property that starts with data.record.""" should "fail in strict interpolator if not bound" in {
    intercept[UnboundAttributeException] {
      interpolateStrict("""Hello ${data.record.Name}!""") {
        case "data.record.Namex" => Some("Gwen")
        case _ => None
      }
    }
  }

  private def interpolate(source: String, lenient: Boolean = false)(resolver: String => Option[String]): String = {
    createInterpolator(lenient, resolver).interpolate(source)
  }

  private def interpolateParams(source: String, lenient: Boolean = false)(resolver: String => Option[String]): String = {
    createInterpolator(lenient, resolver).interpolateParams(source)
  }

  private def createInterpolator(lenient: Boolean, resolver: String => Option[String]): Interpolator = {
    val interpolator = new Interpolator(resolver)
    if (lenient) interpolator.lenient else interpolator
  }

  private def interpolateStrict(source: String, lenient: Boolean = false)(resolver: String => Option[String]): String = {
    createInterpolator(lenient, resolver).strict.interpolate(source)
  }

}
