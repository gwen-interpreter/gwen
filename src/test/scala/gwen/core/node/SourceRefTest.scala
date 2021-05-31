/*
 * Copyright 2014-2021 Branko Juric, Brady Wood
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

package gwen.core.node

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File

class SourceRefTest extends FlatSpec with Matchers {

  val file = new File("path/to/file.feature")

  "toString" should "yield correct string value" in {
    SourceRef(Some(file), 1).toString should be ("path/to/file.feature:1")
    SourceRef(Some(file), 2).toString should be ("path/to/file.feature:2")
  }

  "toString on SourceRef with file, line, and column" should "yield correct string value" in {
    SourceRef.toString(Some(file), Some(1), Some(2)) should be ("path/to/file.feature:1:2")
    SourceRef.toString(Some(file), Some(2), Some(2)) should be ("path/to/file.feature:2:2")
  }

  "toString on SourceRef with file and line" should "yield correct string value" in {
    SourceRef.toString(Some(file), Some(1), None) should be ("path/to/file.feature:1")
    SourceRef.toString(Some(file), Some(2), None) should be ("path/to/file.feature:2")
  }

  "toString on SourceRef with file and column" should "yield correct string value" in {
    SourceRef.toString(Some(file), None, Some(1)) should be ("path/to/file.feature::1")
    SourceRef.toString(Some(file), None, Some(2)) should be ("path/to/file.feature::2")
  }

  "toString on SourceRef with file" should "yield correct string value" in {
    SourceRef.toString(Some(file), None, None) should be ("path/to/file.feature")
    SourceRef.toString(Some(file), None, None) should be ("path/to/file.feature")
  }

  "toString on SourceRef line, and column" should "yield correct string value" in {
    SourceRef.toString(None, Some(1), Some(2)) should be ("line 1 column 2")
    SourceRef.toString(None, Some(2), Some(2)) should be ("line 2 column 2")
  }

  "toString on SourceRef with line" should "yield correct string value" in {
    SourceRef.toString(None, Some(1), None) should be ("line 1")
    SourceRef.toString(None, Some(2), None) should be ("line 2")
  }

  "toString on SourceRef with column" should "yield correct string value" in {
    SourceRef.toString(None, None, Some(1)) should be ("column 1")
    SourceRef.toString(None, None, Some(2)) should be ("column 2")
  }

  "toString on nothing" should "yield blacnk" in {
    SourceRef.toString(None, None, None) should be ("")
    SourceRef.toString(None, None, None) should be ("")
  }  
}
