/*
 * Copyright 2015 Branko Juric, Brady Wood
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

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class InterpolationSupportTest extends FlatSpec with Matchers with InterpolationSupport {

  """interpolate using property syntax: prefix "${property}"""" should "resolve" in {
    interpolate("""hello "${property}"""") { property => "you" } should be ("""hello "you"""") 
  }
  
  """interpolate using property syntax: prefix ${property}""" should "resolve" in {
    interpolate("""hello ${property}""") { property => "you" } should be ("""hello you""") 
  }
  
  """interpolate using property syntax: "${property}" suffix""" should "resolve" in {
    interpolate(""""${property}" you""") { property => "hello" } should be (""""hello" you""") 
  }
  
  """interpolate using property syntax: ${property} "suffix"""" should "resolve" in {
    interpolate("""${property} "you"""") { property => "hello" } should be ("""hello "you"""") 
  }
  
  """interpolate using property syntax: prefix ${property} suffix""" should "resolve" in {
    interpolate("""hello ${property} good thing""") { property => "you" } should be ("""hello you good thing""") 
  }
  
  """interpolate nested using property syntax: ${property1-${property0}}"""" should "resolve" in {
    interpolate("""Hey you ${property-${id}} thing!""") { property => 
      property match {
        case "id" => "0"
        case "property-0" => "good"
        case _ => "undefined"
      }
    } should be ("""Hey you good thing!""") 
  }
  
  """interpolate adjecent values using property syntax: ${property-0} ${property-1}"""" should "resolve" in {
    interpolate("""Hey you ${property-0} ${property-1} thing!""") { property => 
      property match {
        case "id" => "0"
        case "property-0" => "really"
        case "property-1" => "good"
        case _ => "undefined"
      }
    } should be ("""Hey you really good thing!""") 
  }
  
  """interpolate adjecent values using property syntax (no space): ${property-0}${property-1}"""" should "resolve" in {
    interpolate("""Hey you ${property-0}${property-1} thing!""") { property => 
      property match {
        case "id" => "0"
        case "property-0" => "go"
        case "property-1" => "od"
        case _ => "undefined"
      }
    } should be ("""Hey you good thing!""") 
  }
  
  """interpolate using stepdef param syntax: prefix "$<param>"""" should "resolve" in {
    interpolate("""hello "$<param>"""") { param => "you" } should be ("""hello "you"""") 
  }
  
  """interpolate using stepdef param syntax: prefix $<param>""" should "resolve" in {
    interpolate("""hello $<param>""") { param => "you" } should be ("""hello you""") 
  }
  
  """interpolate using stepdef param syntax: "$<param>" suffix""" should "resolve" in {
    interpolate(""""$<param>" you""") { param => "hello" } should be (""""hello" you""") 
  }
  
  """interpolate using stepdef param syntax: $<param> "suffix"""" should "resolve" in {
    interpolate("""$<param> "you"""") { param => "hello" } should be ("""hello "you"""") 
  }
  
  """interpolate using stepdef param syntax: prefix $<param> suffix""" should "resolve" in {
    interpolate("""hello $<param> good thing""") { param => "you" } should be ("""hello you good thing""") 
  }
  
  """interpolate nested using stepdef param syntax: $<param1-$<param0>>"""" should "resolve" in {
    interpolate("""Hey you $<param-$<id>> thing!""") { param => 
      param match {
        case "<id>" => "0"
        case "<param-0>" => "good"
        case _ => "undefined"
      }
    } should be ("""Hey you good thing!""") 
  }
  
  """interpolate stepdef with adjacent params: $<param-0> $<param-1>"""" should "resolve" in {
    interpolate("""Hey you $<param-0> $<param-1> thing!""") { param => 
      param match {
        case "<id>" => "0"
        case "<param-0>" => "really"
        case "<param-1>" => "good"
        case _ => "undefined"
      }
    } should be ("""Hey you really good thing!""") 
  }
  
  """interpolate stepdef with adjacent params (no space): $<param-0>$<param-1>"""" should "resolve" in {
    interpolate("""Hey you $<param-0>$<param-1> thing!""") { param => 
      param match {
        case "<id>" => "0"
        case "<param-0>" => "go"
        case "<param-1>" => "od"
        case _ => "undefined"
      }
    } should be ("""Hey you good thing!""") 
  }
  
  """interpolate using concatentation syntax: prefix "" + binding + " suffix""" should "resolve" in {
    interpolate("""hello "" + binding + " good thing"""") { binding => "you" } should be ("""hello "you good thing"""") 
  }
 
  """interpolate using concatentation syntax: "prefix" + binding""" should "resolve" in {
    interpolate("""hello "" + binding""") { binding => "you" } should be ("""hello "you"""") 
  }
  
}