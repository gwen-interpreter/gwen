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

package gwen.core.node.gherkin

import gwen.core.node.GwenNode
import gwen.core.node.NodeType
import gwen.core.node.SourceRef

import scala.jdk.CollectionConverters._

import io.cucumber.messages.{ Messages => Cucumber }

import java.io.File


/**
  * Captures a gherkin feature node.
  *
  * @param language the language identifier (example: en for English)
  * @param sourceRef the location in source
  * @param tags list of tags
  * @param keyword the Gherkin keyword for this Feature
  * @param name the feature name
  * @param description optional description
  */
case class Feature(
    language: String, 
    sourceRef: Option[SourceRef],
    tags: List[Tag],
    keyword: String, 
    name: String, 
    description: List[String]) extends GherkinNode {

  override val nodeType: NodeType = NodeType.Feature

  override def siblingsIn(parent: GwenNode): List[GwenNode] = {
    parent match {
      case spec: Spec => List(spec.feature)
      case _ => Nil
    }
  }
  
  def specType = if (sourceRef.map(_.uri.contains(".meta")).getOrElse(false)) SpecType.Meta else SpecType.Feature

  def copy(
      withLanguage: String = language, 
      withSourceRef: Option[SourceRef] = sourceRef,
      withTags: List[Tag] = tags,
      withKeyword: String = keyword, 
      withName: String = name, 
      withDescription: List[String] = description): Feature = {
    Feature(withLanguage, withSourceRef, withTags, withKeyword, withName, withDescription)
  }

}

object Feature {
  def apply(file: Option[File], feature: Cucumber.GherkinDocument.Feature): Feature = {
    Feature(
      feature.getLanguage,  
      Option(feature.getLocation).map(loc => SourceRef(file, loc)),
      Option(feature.getTagsList).map(_.asScala.toList).getOrElse(Nil) map { t => Tag(file, t)  },
      feature.getKeyword,
      feature.getName, 
      Option(feature.getDescription).filter(_.length > 0).map(_.split("\n").toList.map(_.trim)).getOrElse(Nil).distinct
    )
  }
}
