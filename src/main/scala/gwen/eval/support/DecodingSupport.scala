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

import gwen.errors._
import gwen.eval.EnvContext
import org.apache.commons.codec.binary.Base64

/** Can be mixed into evaluation engines to provide decoding support. */
trait DecodingSupport {
  this: EnvContext =>

  /**
    * Decodes a base 64 encoded string.
    * 
    * @param source the encoded source string
    * @return the decoded result string
    */
  def decodeBase64(source: String): String =
    evaluate("$[dryRun:decodeBase64]") {
      Option(source) match {
        case None =>
          decodingError("Cannot Base64 decode null string")
        case Some(_) =>
          new String(Base64.decodeBase64(source.getBytes()))
      }
    }
  
}