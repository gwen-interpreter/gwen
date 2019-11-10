/*
 * Copyright 2017 Branko Juric, Brady Wood
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

package gwen.dsl

import scala.language.postfixOps

object FeatureKeyword extends Enumeration {
  type FeatureKeyword = Value
  val Feature, Background, Rule, Scenario, Example, Examples = Value

  /** List of all keyword names. */
  val names: Set[String] = values.map(_.toString)

}

object FeatureSymbol extends Enumeration {
  type FeatureSymbol = Value
  val `#` = Value("#")
  val `@` = Value("@")

  /** List of all keyword names. */
  val names: Set[String] = values.map(_.toString)

}

object StepKeyword extends Enumeration {

  val Given, When, Then, And, But = Value

  /** List of all keyword names. */
  val names: Set[String] = values.map(_.toString)

}

object ReservedKeyword {
  val literals = FeatureKeyword.names.map(_ + ":") ++ FeatureSymbol.names ++ StepKeyword.names
}

object AssertionMode extends Enumeration {
  type FeatureKeyword = Value
  val hard, soft, sustained = Value
}

