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

package gwen.core.model

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File

class SourceRefTest extends FlatSpec with Matchers {

  "toString" should "yield correct string value" in {
    SourceRef("path/to/file.feature", Position(1, 2, Nil)).toString() should be ("path/to/file.feature:1:2")
    SourceRef("path/to/file.feature", Position(1, 2, List(0))).toString() should be ("path/to/file.feature:1:2")
    SourceRef("path/to/file.feature", Position(1, 2, List(0, 1))).toString() should be ("path/to/file.feature:1:2[1][2]")
  }

  "asString on SourceRef with file and ref" should "yield correct string value" in {
    val file = new File("path/to/file.feature")
    SourceRef.asString(Some(file), Some(SourceRef("", Position(1, 2, Nil)))).toString() should be ("path/to/file.feature:1:2")
    SourceRef.asString(Some(file), Some(SourceRef("", Position(1, 2, List(0))))).toString() should be ("path/to/file.feature:1:2")
    SourceRef.asString(Some(file), Some(SourceRef("", Position(1, 2, List(0, 1))))).toString() should be ("path/to/file.feature:1:2[1][2]")
  }

  "asString on SourceRef with no file and ref" should "yield correct string value" in {
    SourceRef.asString(None, Some(SourceRef("path/to/file.feature", Position(1, 2, Nil)))).toString() should be ("path/to/file.feature:1:2")
    SourceRef.asString(None, Some(SourceRef("path/to/file.feature", Position(1, 2, List(0))))).toString() should be ("path/to/file.feature:1:2")
    SourceRef.asString(None, Some(SourceRef("path/to/file.feature", Position(1, 2, List(0, 1))))).toString() should be ("path/to/file.feature:1:2[1][2]")
  }

  "asString on SourceRef with no file and no ref" should "yield blank" in {
    SourceRef.asString(None, None).toString() should be ("")
  }

  "asString on SourceRef with file and no ref" should "yield correct string value" in {
    val file = new File("path/to/file.feature")
    SourceRef.asString(Some(file), None).toString() should be ("path/to/file.feature")
  }

  "asString on uri, line, column and table row" should "yield correct string value" in {
    SourceRef.asString(Some("path/to/file.feature"), Some(1), Some(2), Some((1, 2))).toString() should be ("path/to/file.feature:1:2[1][2]")
  }

  "asString on uri, line and column" should "yield correct string value" in {
    SourceRef.asString(Some("path/to/file.feature"), Some(1), Some(2), None).toString() should be ("path/to/file.feature:1:2")
  }

  "asString on uri, line and table row" should "yield correct string value" in {
    SourceRef.asString(Some("path/to/file.feature"), Some(1), None, Some((1, 2))).toString() should be ("path/to/file.feature:1[1][2]")
  }

  "asString on uri and line" should "yield correct string value" in {
    SourceRef.asString(Some("path/to/file.feature"), Some(1), None, None).toString() should be ("path/to/file.feature:1")
  }

  "asString on uri, column and table row" should "yield correct string value" in {
    SourceRef.asString(Some("path/to/file.feature"), None, Some(2), Some((1, 2))).toString() should be ("path/to/file.feature:_:2[1][2]")
  }

  "asString on uri and column" should "yield correct string value" in {
    SourceRef.asString(Some("path/to/file.feature"), None, Some(2), None).toString() should be ("path/to/file.feature:_:2")
  }

  "asString on uri" should "yield correct string value" in {
    SourceRef.asString(Some("path/to/file.feature"), None, None, None).toString() should be ("path/to/file.feature")
  }

  "asString on line, column and table row" should "yield correct string value" in {
    SourceRef.asString(None, Some(1), Some(2), Some((1, 2))).toString() should be (":1:2[1][2]")
  }

  "asString on line and column" should "yield correct string value" in {
    SourceRef.asString(None, Some(1), Some(2), None).toString() should be (":1:2")
  }

  "asString on line and table row" should "yield correct string value" in {
    SourceRef.asString(None, Some(1), None, Some((1, 2))).toString() should be (":1[1][2]")
  }

  "asString on line" should "yield correct string value" in {
    SourceRef.asString(None, Some(1), None, None).toString() should be (":1")
  }

  "asString on column and table row" should "yield correct string value" in {
    SourceRef.asString(None, None, Some(2), Some((1, 2))).toString() should be (":_:2[1][2]")
  }

  "asString on column" should "yield correct string value" in {
    SourceRef.asString(None, None, Some(2), None).toString() should be (":_:2")
  }

  "asString on nothing" should "yield blank" in {
    SourceRef.asString(None, None, None, None).toString() should be ("")
  }
  
}
