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
import gwen.eval.EnvContext
import gwen.errors.DecodingException
import gwen.eval.{GwenOptions, ScopedDataStack}

class DecodingSupportTest extends FlatSpec with Matchers {

  val decodingSupport: DecodingSupport = new EnvContext(GwenOptions())

  "Null Base64 source" should "throw error" in {
    intercept[DecodingException] {
      decodingSupport.decodeBase64(null)
    }
  }
  
  "Empty Base64 source" should "return empty string" in {
    decodingSupport.decodeBase64("") should be ("")
  }
  
  "Space Base64 source" should "return empty string" in {
    decodingSupport.decodeBase64(" ") should be ("")
  }
  
  "Valid Base64 source" should "decode correctly" in {
    val source = "WW91IGNhbiB0ZXN0IGZvciB0aGUgcHJlc2VuY2Ugb2YgYnVncyBidXQgbmV2ZXIgdGhlaXIgYWJzZW5jZSE="
    val target = "You can test for the presence of bugs but never their absence!"
    decodingSupport.decodeBase64(source) should be (target)
  }
  
}