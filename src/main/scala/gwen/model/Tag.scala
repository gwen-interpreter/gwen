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

package gwen.model

import gwen._
import gwen.model._

import io.cucumber.messages.{ Messages => Cucumber }

/**
  * Captures a gherkin tag.
  *
  * @param sourceRef the location in source
  * @param name name the tag name
  */
case class Tag(sourceRef: Option[SourceRef], name: String, value: Option[String]) extends SpecNode {
  
  def nodeType: NodeType.Value = NodeType.Tag

  if (name.matches("""\s""")) {
    Errors.invalidTagError(s"Whitespace not allowed in tag name '$name'")
  }
  value foreach { v=>
    if (v.matches("""\s""")) {
      Errors.invalidTagError(s"Whitespace not allowed in @$name tag value '$v'")
    }
  }
    
  /** Returns a string representation of this tag. */
  override def toString: String = s"@$name${value.map(v => s"""("$v")""").getOrElse("")}"

  def copy(
      withSourceRef: Option[SourceRef] = sourceRef,
      withName: String = name,
      withValue: Option[String] = value): Tag = {
    Tag(withSourceRef, withName, withValue)
  }
  
}

object Tag {

  def apply(uri: String, tag: Cucumber.GherkinDocument.Feature.Tag, index: Int): Tag = {
    val pos = Option(tag.getLocation).map(loc => SourceRef(uri, loc, index))
    Tag(pos, tag.getName) tap { t =>
      if (t.name == ReservedTags.DataTable.toString) {
        DataTable.checkTagSyntax(t)
      }
    }
  }

  def apply(name: ReservedTags.Value): Tag = {
    Tag(None, name.toString, None)
  }
  def apply(name: ReservedTags.Value, value: String): Tag = {
    Tag(None, name.toString, Option(value))
  }
  def apply(tagString: String): Tag = {
    apply(None, tagString)
  }
  def apply(sourceRef: Option[SourceRef], tagString: String): Tag = {
    if (tagString.matches("""\s""")) {
      Errors.invalidTagError(s"Whitespace not allowed in tag '$tagString'")
    }
    tagString match {
      case r"""~?@(.*?)$n\("(.*?)"$v\)""" => Tag(sourceRef, n, Option(v))
      case r"""~?@(.*?)$n""" => Tag(sourceRef, n, None)
      case _ => Errors.invalidTagError(s"Invalid tag syntax: $tagString")
    }
  }
  def findTagValue(tags: List[Tag], name: String): Option[String] = {
    findByName(tags, name).find(_.name == name).flatMap(_.value)
  }
  def findByName(tags: List[Tag], name: String): Option[Tag] = {
    findAllByName(tags, name).headOption
  }
  def findAllByName(tags: List[Tag], name: String): List[Tag] = {
    tags.filter(_.name == name)
  }
  
}
