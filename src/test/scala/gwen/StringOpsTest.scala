/*
 * Copyright 2018 Branko Juric, Brady Wood
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

package gwen

import gwen.StringOps.lastPositionIn
import gwen.model.Position

import org.scalatest.{FlatSpec, Matchers}

class StringOpsTest extends FlatSpec with Matchers {

  "empty string" should " return (1, 1)" in {
    lastPositionIn("") should be (Position(1, 1, 0))
  }

  "empty line" should " return (2, 1)" in {
    lastPositionIn(
      """
        |""".stripMargin) should be (Position(2, 1, 0))
  }

  "single character" should " return (1, 1)" in {
    lastPositionIn("x") should be (Position(1, 1, 0))
  }

  "single character line" should " return (2, 1)" in {
    lastPositionIn(
      """x
        |""".stripMargin) should be (Position(2, 1, 0))
  }

  "multi-character single line" should " return (1, 5)" in {
    lastPositionIn("howdy") should be (Position(1, 5, 0))
  }

  "multi-character single line followed by new line" should " return (2, 1)" in {
    lastPositionIn(
      """howdy
        |""".stripMargin) should be (Position(2, 1, 0))
  }

  "multiline non-empty last line" should " return last position" in {
    lastPositionIn(
      """First line
        |Second line""".stripMargin) should be (Position(2, 11, 0))
  }

  "multiline empty last line" should " return last position" in {
    lastPositionIn(
      """First line
        |Second line
        |""".stripMargin) should be (Position(3, 1, 0))
  }

}
