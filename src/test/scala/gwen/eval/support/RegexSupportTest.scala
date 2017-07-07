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
import gwen.eval.{EnvContext, GwenOptions, ScopedDataStack}

class RegexSupportTest extends FlatSpec with Matchers {

  val regexSuport: RegexSupport = new EnvContext(GwenOptions(), new ScopedDataStack())

  """id=(\d+) on http://host:post/path?id=1""" should "return 1" in {
    regexSuport.extractByRegex("""id=(\d+)""", "http://host:post/path?id=1") should be ("1")
  }
  
  """id=(\d+) on http://host:post/path?id=1&name=gwen""" should "return 1" in {
    regexSuport.extractByRegex("""id=(\d+)""", "http://host:post/path?id=1&name=gwen") should be ("1")
  }
  
  """id=(\d+) on http://host:post/path?value=x&id=1&name=gwen""" should "return 1" in {
    regexSuport.extractByRegex("""id=(\d+)""", "http://host:post/path?value=x&id=1&name=gwen") should be ("1")
  }
  
}