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

 package gwen.core.state

import gwen.core.node.gherkin.ReservedTags

object ReservedParam extends Enumeration {

  val `ForEach.name` = Value(s"${ReservedTags.ForEach}.name")
  val `ForEach.index` = Value(s"${ReservedTags.ForEach}.index")
  val `ForEach.iteration` = Value(s"${ReservedTags.ForEach}.iteration")
  val `While.iteration` = Value(s"${ReservedTags.While}.iteration")
  val `Until.iteration` = Value(s"${ReservedTags.Until}.iteration")

  val names: List[String] = values.toList.filter(_.toString.endsWith(".name")).map(_.toString)
  val indexes: List[String] = values.toList.filter(_.toString.endsWith(".index")).map(_.toString)
  val iterations: List[String] = values.toList.filter(_.toString.endsWith(".iteration")).map(_.toString)

} 