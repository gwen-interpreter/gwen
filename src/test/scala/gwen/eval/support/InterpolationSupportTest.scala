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

  """interpolate substitution: prefix "${binding}"""" should "resolve" in {
    interpolate("""hello "${binding}"""") { binding => "you" } should be ("""hello "you"""") 
  }
  
  """interpolate substitution: prefix ${binding}""" should "resolve" in {
    interpolate("""hello ${binding}""") { binding => "you" } should be ("""hello you""") 
  }
  
  """interpolate substitution: "${binding}" suffix""" should "resolve" in {
    interpolate(""""${binding}" you""") { binding => "hello" } should be (""""hello" you""") 
  }
  
  """interpolate substitution: ${binding} "suffix"""" should "resolve" in {
    interpolate("""${binding} "you"""") { binding => "hello" } should be ("""hello "you"""") 
  }
  
  """interpolate substitution: prefix ${binding} suffix""" should "resolve" in {
    interpolate("""hello ${binding} good thing""") { binding => "you" } should be ("""hello you good thing""") 
  }
  
  """interpolate concatenation: prefix "" + binding + " suffix""" should "resolve" in {
    interpolate("""hello "" + binding + " good thing"""") { binding => "you" } should be ("""hello "you good thing"""") 
  }
 
  """interpolate concatenation: "prefix" + binding""" should "resolve" in {
    interpolate("""hello "" + binding""") { binding => "you" } should be ("""hello "you"""") 
  }
  
}