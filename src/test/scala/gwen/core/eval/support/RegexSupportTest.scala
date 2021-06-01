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

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class RegexSupportTest extends FlatSpec with Matchers with RegexSupport {

  """id=(\d+) on http://host:post/path?id=1""" should "return 1" in {
    extractByRegex("""id=(\d+)""", "http://host:post/path?id=1") should be ("1")
  }
  
  """id=(\d+) on http://host:post/path?id=1&name=gwen""" should "return 1" in {
    extractByRegex("""id=(\d+)""", "http://host:post/path?id=1&name=gwen") should be ("1")
  }
  
  """id=(\d+) on http://host:post/path?value=x&id=1&name=gwen""" should "return 1" in {
    extractByRegex("""id=(\d+)""", "http://host:post/path?value=x&id=1&name=gwen") should be ("1")
  }
  
}