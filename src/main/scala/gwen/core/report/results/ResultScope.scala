/*
 * Copyright 2024 Branko Juric, Brady Wood
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

package gwen.core.report.results

import gwen.core._
import gwen.core.node.NodeType

import scala.util.Success
import scala.util.Try

import java.io.File

case class ResultScope(nodeType: NodeType, nodeName: Option[String])

object ResultScope {
  
  private val validNodes = List(NodeType.Feature, NodeType.Rule, NodeType.Scenario, NodeType.Examples, NodeType.StepDef)
  private val scopePattern = s"""(${validNodes.mkString("|")})\\s*:\\s*(.*)""".r
  private val validNodeValues = (validNodes.map(_.toString) ++ validNodes.map(_.toString + ": <name>")).mkString(", ")

  def resolve(settingName: String): Option[ResultScope] = {
    Settings.getOpt(settingName) map { nodeValue =>
      Try(NodeType.valueOf(nodeValue.trim)) match {
        case Success(n) =>
          if (validNodes.contains(n)) {
            ResultScope(n, None)
          } else {
            Errors.illegalSettingError(settingName, nodeValue, validNodes.mkString(", "))
          }
        case _ =>
          nodeValue.trim match {
            case scopePattern(node, name) =>
              Try(NodeType.valueOf(node)) match {
                case Success(n) =>
                  ResultScope(n, Some(name))
                case _ =>
                  Errors.illegalSettingError(settingName, nodeValue, validNodeValues)
              }
            case _ =>
              Errors.illegalSettingError(settingName, nodeValue, validNodeValues)
          }
      }
    }
  }
}