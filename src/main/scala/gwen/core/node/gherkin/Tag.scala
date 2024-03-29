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

import gwen.core._
import gwen.core.node.GwenNode
import gwen.core.node.NodeType
import gwen.core.node.SourceRef
import gwen.core.node.gherkin.table.DataTable

import scala.util.chaining._

import io.cucumber.messages.{ types => cucumber }

import java.io.File

/**
  * Captures a gherkin tag.
  *
  * @param sourceRef the location in source
  * @param name name the tag name
  */
case class Tag(sourceRef: Option[SourceRef], name: String, value: Option[String]) extends GherkinNode {

  override val nodeType: NodeType = NodeType.Tag
  
  def isAnnotation = value.nonEmpty || Annotations.values.filter(_ != Annotations.Ignore).exists(_.toString == name)
  def isMarker = value.isEmpty && !isAnnotation

  override def siblingsIn(parent: GwenNode): List[GwenNode] = {
    parent match {
      case feature: Feature => feature.tags
      case scenario: Scenario => scenario.tags
      case examples: Examples => examples.tags
      case _ => Nil
    }
  }

  if (name.matches("""\s""")) {
    Errors.invalidTagError(s"Whitespace not allowed in tag name '$name'")
  }
  value foreach { v=>
    if (v.matches("""\s""")) {
      Errors.invalidTagError(s"Whitespace not allowed in @$name tag value '$v'")
    }
  }

  /** Returns a string representation of this tag. */
  override def toString: String = s"@$name${value.map(v => s"""(${Formatting.surroundWithQuotesForAnnotation(v)})""").getOrElse("")}"

  def copy(
      withSourceRef: Option[SourceRef] = sourceRef,
      withName: String = name,
      withValue: Option[String] = value): Tag = {
    Tag(withSourceRef, withName, withValue)
  }

  /**
    * Interpolate placeholder references in this tag.
    *
    * @param interpolator the interpolator to use
    * @return the interpolated step
    */
  override def interpolate(interpolator: String => String): Tag = {
    copy(
      withName = interpolator.apply(name),
      withValue = value.map(interpolator)
    )
  }

}

object Tag {

  def apply(file: Option[File], tag: cucumber.Tag): Tag = {
    val pos = Option(tag.getLocation).map(loc => SourceRef(file, loc))
    Tag(pos, tag.getName) tap { t =>
      if (t.name == Annotations.DataTable.toString) {
        DataTable.checkTagSyntax(t)
      }
    }
  }

  def apply(name: Annotations): Tag = {
    Tag(None, name.toString, None)
  }
  def apply(name: Annotations, value: String): Tag = {
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
      case r"""~?@(.*?)$n\('(.*?)'$v\)""" => Tag(sourceRef, n, Option(v))
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

  def parseSingleValue(sourceRef: Option[SourceRef], annotation: Annotations, name: Option[String], value: String): String = {
    parseSingleValue(sourceRef, annotation.toString, name, value)
  }

  private def parseSingleValue(sourceRef: Option[SourceRef], annotation: String, name: Option[String], value: String): String = {
    value.trim match {
      case r"""('|"|`)$q(.+?)$v\1""" => v
      case _ => Errors.invalidAnnotationError(sourceRef, s"${name.getOrElse("value")} in @$annotation annotation must be surrounded by single (preferred) or double quotes")
    }
  }

  def parseListValue(sourceRef: Option[SourceRef], annotation: Annotations, name: Option[String], value: String): List[String] = {
    def parseList(csv: String): List[String] = {
      csv.split(",").toList map { v => 
        parseSingleValue(sourceRef, annotation, name.map(n => s"$n entry"), v)
      }
    }
    value.trim match {
      case r"""\[(.+?)\]$csv""" => parseList(csv)
      case r"""\{(.+?)\}$csv""" => parseList(csv)
      case r"""'(.+?)$v'""" => List(v)
      case r""""(.+?)$v"""" => List(v)
      case _ => Errors.invalidAnnotationError(sourceRef, s"${name.getOrElse("value")} in @$annotation annotation must be surrounded by single or double quotes for a single value or square or curly braces for a list value")
    }
  }

}
