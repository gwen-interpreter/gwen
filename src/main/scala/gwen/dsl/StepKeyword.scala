/*
 * Copyright 2014-2015 Branko Juric, Brady Wood
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

/**
  * Enumeration of supported step keywords.  Every [[Step]] 
  * expression starts with one of the keywords defined here.
  * 
  * @author Branko Juric
  */
object StepKeyword extends Enumeration {

  val Given, When, Then, And, But = Value

  /**
    * Map of step keywords keyed by name. This can be used to lookup
    * keywords by name.
    */
  val names: Map[String, _root_.gwen.dsl.StepKeyword.Value] = values map { k => k.toString -> k } toMap
  
  /** List of all keyword string literals. */
  val literals: List[String] = List(Given, When, Then, And, But).map(_.toString)

}

