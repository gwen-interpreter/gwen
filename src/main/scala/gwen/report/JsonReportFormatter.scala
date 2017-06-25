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

import java.io.File

import gwen.GwenInfo
import gwen.dsl._
import gwen.Predefs.Formatting._
import gwen.Predefs.FileIO
import gwen.eval.{FeatureResult, FeatureSummary, FeatureUnit, GwenOptions}
import org.apache.commons.codec.binary.Base64

import scala.util.Properties

/** Formats the feature summary and detail reports in JSON format. */
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

    val scenarios = result.spec.scenarios.filter(!_.isStepDef).flatMap { scenario =>
      if (scenario.isOutline) {
        if (EvalStatus.isEvaluated(scenario.evalStatus.status)) scenario.examples.flatMap(_.scenarios).map((_, true))
        else List((scenario, true))
      }
      else List((scenario, false))
    }
    val spec = result.spec
    val feature = spec.feature
    val featureId = s"${result.spec.featureFile.map(f => FileIO.encodeDir(s"${f.getPath}.")).getOrElse("")}${feature.name.toLowerCase.replace(' ', '-')}"
    val featureName = s"${result.spec.featureFile.map(f => s"${f.getPath}: ").getOrElse("")}${feature.name}"
    
    Some(s"""[
  {${spec.featureFile.map(file => s"""
    "uri": "${escapeJson(file.getPath)}",""").getOrElse("")}
    "keyword": "${Feature.keyword}",
    "id": "${escapeJson(featureId)}",
    "line": ${feature.pos.line},
    "name": "${escapeJson(featureName)}",
    "description": "${feature.description.map(escapeJson).mkString("\\n")}"${if(feature.tags.nonEmpty) s""",
    "tags": [${feature.tags.filter(!_.name.startsWith("Import(")).map { tag => s"""
      {
        "name": "${escapeJson(tag.toString)}",
        "line": ${tag.pos.line}
      }"""}.mkString(",")}
    ]""" else ""}${if(spec.scenarios.nonEmpty) s""",
    "elements": [${scenarios.zipWithIndex.map { case ((scenario, isOutline), idx) =>
        s"${scenario.background.map(bg => s"${renderBackground(bg, idx)},${Properties.lineSeparator}").getOrElse("")}${renderScenario(scenario, isOutline, idx)}"
      }.mkString(",")
    }
    ]""" else ""}
  }
]""")
  }

  private def renderBackground(background: Background, idx: Int) = s"""
      {
        "keyword": "${Background.keyword}",
        "id": "scenario-${idx + 1}-background-${background.name.toLowerCase.replace(' ', '-')}",
        "line": ${background.pos.line},
        "name": "${escapeJson(background.name)}",
        "description": "${background.description.map(escapeJson).mkString("\\n")}",
        "type": "${Background.keyword.toLowerCase}"${if (background.steps.nonEmpty) s""",
        "steps": [${renderSteps(background.steps)}
        ]""" else ""}
      }"""

  private def renderScenario(scenario: Scenario, isOutline: Boolean, idx: Int) = s"""
      {
        "keyword": "${scenario.keyword}${if (isOutline) " Outline" else ""}",
        "id": "${scenario.keyword.toLowerCase.replace(" ", "-")}-${idx + 1}-${scenario.name.toLowerCase.replace(' ', '-')}",
        "line": ${scenario.pos.line},
        "name": "${escapeJson(scenario.name)}",
        "description": "${scenario.description.map(escapeJson).mkString("\\n")}"${if(scenario.tags.nonEmpty) s""",
        "tags": [${scenario.tags.map { case tag => s"""
          {
            "name": "${escapeJson(tag.toString)}",
            "line": ${tag.pos.line}
          }"""}.mkString(",")}
        ]""" else ""},
        "type": "${scenario.keyword.toLowerCase.replace(" ", "_")}"${if (scenario.steps.nonEmpty) s""",
        "steps": [${renderSteps(scenario.steps)}
        ]""" else ""}${if (isOutline && scenario.keyword == "Scenario Outline" && scenario.examples.nonEmpty) s""",
        "examples": [${scenario.examples.map { examples => s"""
           {
             "keyword": "${Examples.keyword}",
             "name": "${escapeJson(examples.name)}",
             "line": ${examples.pos.line},
             "description": "${examples.description.map(escapeJson).mkString("\\n")}",
             "id": "${scenario.keyword.toLowerCase.replace(" ", "-")}-${idx + 1}-${scenario.name.toLowerCase.replace(' ', '-')}--${examples.name.toLowerCase.replace(' ', '-')}",
             "rows": [${examples.table.zipWithIndex.map { case ((line, table), eIndex) => s"""
               {
                 "cells": [${table.map(value => s"""
                   "$value"""").mkString(",")}
                 ],
                 "line": ${line},
                 "id": "${scenario.keyword.toLowerCase.replace(" ", "-")}-${idx + 1}-${scenario.name.toLowerCase.replace(' ', '-')}--${examples.name.toLowerCase.replace(' ', '-')}-${eIndex + 1}"
               }"""}.mkString(",")}
             ]
           }"""}.mkString(",")}

        ]""" else ""}
      }"""

  private def renderSteps(steps: List[Step]) = steps.map { step =>
    val screenshots = step.attachments.filter(_._1 == "Screenshot").map(_._2)
    s"""
          {
            "keyword": "${step.keyword} ",
            "name": "${escapeJson(step.expression)}",
            "line": ${step.pos.line},${if (screenshots.nonEmpty) s"""
            "embeddings": [${screenshots.map{ file => s"""
              {
                "mime_type": "${file.mimeType}",
                "data": "${Base64.encodeBase64String(file.readBytes)}"
              }"""}.mkString(",")}
            ],""" else ""}${step.stepDef.map { stepDef => if (stepDef.metaFile.nonEmpty) s"""
            "match": {
                "location": "${stepDef.metaFile.get.getPath}:${stepDef.steps.headOption.map(_.pos.line - stepDef.description.size - 1).getOrElse(1)}"
            },""" else ""}.getOrElse("")}
            "result": {
              "status": "${step.status.status.toString.toLowerCase}",${if(step.status.isInstanceOf[Failed]) s"""
              "error_message": "${escapeJson(step.status.asInstanceOf[Failed].error.getMessage)}",""" else ""}
              "duration": ${step.status.nanos}
            }
          }"""}.mkString(",")

  /**
    * Formats the feature summary report as HTML.
    * 
    * @param options gwen command line options
    * @param info the gwen implementation info
    * @param summary the accumulated feature results summary
    */
  override def formatSummary(options: GwenOptions, info: GwenInfo, summary: FeatureSummary): Option[String] = None
  
}