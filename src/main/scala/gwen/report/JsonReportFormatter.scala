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
package gwen.report

import gwen.dsl._
import gwen.FileIO
import gwen.GwenInfo
import gwen.Formatting.escapeJson
import gwen.eval.FeatureResult
import gwen.eval.FeatureUnit
import gwen.eval.FeatureSummary
import gwen.eval.GwenOptions

import scala.util.Properties

import org.apache.commons.codec.binary.Base64

import java.io.File

/** Formats the feature summary and detail reports in cucumber compliant JSON format. */
trait JsonReportFormatter extends ReportFormatter {
  
  /**
    * Formats the feature detail report as JSON.
    * 
    * @param options gwen command line options
    * @param info the gwen implementation info
    * @param unit the feature input
    * @param result the feature result to report
    * @param breadcrumbs names and references for linking back to parent reports
    * @param reportFiles the target report files (head = detail, tail = metas)
    */
  override def formatDetail(options: GwenOptions, info: GwenInfo, unit: FeatureUnit, result: FeatureResult, breadcrumbs: List[(String, File)], reportFiles: List[File]): Option[String] = {

    val scenarios = result.spec.evalScenarios.filter(!_.isStepDef).flatMap { scenario =>
      if (scenario.isOutline) {
        if (EvalStatus.isEvaluated(scenario.evalStatus.status)) scenario.examples.flatMap(_.scenarios).map((_, true))
        else List((scenario, false))
      }
      else List((scenario, false))
    }
    val spec = result.spec
    val feature = spec.feature

    val id = s"${result.spec.featureFile.map(f => FileIO.encodeDir(s"${f.getPath};")).getOrElse("")}${feature.name.toLowerCase.replace(' ', '-')}"
    val name = s"${result.spec.featureFile.map(f => s"${f.getPath}: ").getOrElse("")}${feature.name}"
    val description = s"${feature.description.mkString(Properties.lineSeparator)}"
    
    Some(s"""[
  {${spec.featureFile.map(file => s"""
    "uri": "${escapeJson(file.getPath)}",""").getOrElse("")}
    "keyword": "${feature.keyword}",
    "id": "${escapeJson(id)}"${feature.sourceRef map { loc => s""",
    "line": ${loc.pos.line}""" } getOrElse("")},
    "name": "${escapeJson(name)}",
    "description": "${escapeJson(description)}"${if(feature.tags.nonEmpty) s""",
    "tags": [${feature.tags.filter(_.value.isEmpty).map { tag => s"""
      {
        "name": "${escapeJson(tag.toString)}"${tag.sourceRef map { loc => s""",
        "line": ${loc.pos.line}""" } getOrElse("")},
      }"""}.mkString(",")}
    ]""" else ""}${if(scenarios.nonEmpty) s""",
    "elements": [${scenarios.zipWithIndex.map { case ((scenario, isExpanded), idx) =>
        s"${scenario.background.map(bg => s"${renderBackground(bg, idx)},${Properties.lineSeparator}").getOrElse("")}${renderScenario(scenario, isExpanded, idx)}"
      }.mkString(",")
    }
    ]""" else ""}
  }
]""")
  }

  private def renderBackground(background: Background, bIndex: Int) = {
    val id = s"${background.keyword.toLowerCase};${background.name.toLowerCase.replace(' ', '-')};${bIndex + 1}"
    val description = s"${background.description.mkString(Properties.lineSeparator)}"
    s"""
      {
        "keyword": "${background.keyword}",
        "id": "${escapeJson(id)}"${background.sourceRef.map { loc => s""",
        "line": ${loc.pos.line}""" } getOrElse("")},
        "name": "${escapeJson(background.name)}",
        "description": "${escapeJson(description)}",
        "type": "${background.keyword.toLowerCase}"${if (background.steps.nonEmpty) s""",
        "steps": [${renderSteps(background.steps, id)}
        ]""" else ""}
      }"""
  }

  private def renderScenario(scenario: Scenario, isExpanded: Boolean, sIndex: Int) = {
    val keyword = s"${scenario.keyword}${if (isExpanded) " Outline" else ""}"
    val scenarioId = s"${keyword.toLowerCase.replace(' ', '-')};${scenario.name.toLowerCase.replace(' ', '-')};${sIndex + 1}"
    val description = s"${scenario.description.mkString(Properties.lineSeparator)}"
    s"""
      {
        "keyword": "$keyword",
        "id": "${escapeJson(scenarioId)}"${scenario.sourceRef map { loc => s""",
        "line": ${loc.pos.line}""" } getOrElse("")},
        "name": "${escapeJson(scenario.name)}",
        "description": "${escapeJson(description)}"${if(scenario.tags.nonEmpty) s""",
        "tags": [${scenario.tags.map { case tag => s"""
          {
            "name": "${escapeJson(tag.toString)}"${tag.sourceRef map { loc => s""",
            "line": ${loc.pos.line}""" } getOrElse("")},
          }"""}.mkString(",")}
        ]""" else ""},
        "type": "${scenario.keyword.toLowerCase.replace(" ", "_")}"${if (scenario.steps.nonEmpty) s""",
        "steps": [${renderSteps(scenario.steps, scenarioId)}
        ]""" else ""}${if (!isExpanded && scenario.examples.nonEmpty) s""",
        "examples": [${scenario.examples.zipWithIndex.map { case (examples, eIndex) =>
          val examplesId = s"$scenarioId;${examples.name.toLowerCase.replace(' ', '-')};${eIndex + 1}"
          val examplesDescription = s"${examples.description.mkString(Properties.lineSeparator)}"
          s"""
           {
             "keyword": "${examples.keyword}",
             "name": "${escapeJson(examples.name)}"${examples.sourceRef map { loc => s""",
             "line": ${loc.pos.line}"""} getOrElse("")},
             "description": "${escapeJson(examplesDescription)}",
             "id": "${escapeJson(examplesId)}",
             "rows": [${examples.table.zipWithIndex.map { case ((line, table), rIndex) =>
               val rowId = s"$examplesId;${rIndex + 1}"
               s"""
               {
                 "cells": [${table.map(value => s"""
                   "${escapeJson(value)}"""").mkString(",")}
                 ],
                 "line": $line,
                 "id": "${escapeJson(rowId)}"
               }"""}.mkString(",")}
             ]
           }"""}.mkString(",")}
        ]""" else ""}
      }"""
  }

  private def renderSteps(steps: List[Step], parentId: String) = steps.map { step =>
    val screenshots = step.deepAttachments.filter(_._1 == "Screenshot").map(_._2)
    s"""
          {
            "keyword": "${step.keyword} ",
            "name": "${escapeJson(step.name)}"${step.sourceRef.map { loc => s""",
            "line": ${loc.pos.line}"""} getOrElse("")},${if (screenshots.nonEmpty) s"""
            "embeddings": [${screenshots.map{ file => s"""
              {
                "mime_type": "${escapeJson(file.mimeType)}",
                "data": "${escapeJson(Base64.encodeBase64String(file.readBytes))}"
              }"""}.mkString(",")}
            ],""" else ""}${step.stepDef.map { sd =>
              if (sd.sourceRef.nonEmpty) {
                s"""
            "match": {
                "location": "${escapeJson(sd.sourceRef.get.toString)}"
            },"""} else ""}.getOrElse("")}${step.docString.map{ case (line, content, contentType) => s"""
            "doc_string": {
              "content_type": "${contentType.map(c => escapeJson(c)).getOrElse("")}",
              "value": "${escapeJson(content)}",
              "line": $line
            },"""}.getOrElse("")}${if (step.table.nonEmpty) s"""
            "rows": [${step.table.zipWithIndex.map { case ((line, row), rIndex) =>
              val rowId = s"$parentId;${rIndex + 1};$line"
              s"""
              {
                "cells": [${row.map(value => s"""
                  "${escapeJson(value)}"""").mkString(",")}
                ],
                "line": $line,
                "id": "${escapeJson(rowId)}"
              }"""}.mkString(",")}
            ],""" else ""}
            "result": {
              "status": "${step.evalStatus.status.toString.toLowerCase}",${if(step.evalStatus.isInstanceOf[Failed]) s"""
              "error_message": "${escapeJson(step.evalStatus.asInstanceOf[Failed].error.getMessage)}",""" else ""}
              "duration": ${step.evalStatus.nanos}
            }
          }"""}.mkString(",")

  /**
    * Formats the feature summary report as JSON (this implementation does nothing).
    * 
    * @param options gwen command line options
    * @param info the gwen implementation info
    * @param summary the accumulated feature results summary
    */
  override def formatSummary(options: GwenOptions, info: GwenInfo, summary: FeatureSummary): Option[String] = None
  
}