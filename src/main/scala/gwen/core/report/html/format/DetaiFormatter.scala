/*
 * Copyright 2021 Branko Juric, Brady Wood
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
package gwen.core.report.html.format

import gwen.core._
import gwen.core.Formatting._
import gwen.core.node.FeatureUnit
import gwen.core.node.NodeType
import gwen.core.node.gherkin.Background
import gwen.core.node.gherkin.Examples
import gwen.core.node.gherkin.Rule
import gwen.core.node.gherkin.Scenario
import gwen.core.node.gherkin.Step
import gwen.core.report.html.HtmlReportConfig
import gwen.core.result.ResultsSummary
import gwen.core.result.SpecResult
import gwen.core.status._

import scala.io.Source
import scalatags.Text.all._
import scalatags.Text.TypedTag

import java.io.File
import gwen.core.node.gherkin.SpecType
import gwen.core.node.gherkin.GherkinNode

/** Formats the feature summary and detail reports in HTML. */
trait DetaiFormatter {
  this: HtmlReportFormatter =>

  import HtmlReportFormatter._

  /**
    * Formats the feature detail report as HTML.
    *
    * @param options     gwen command line options
    * @param unit        the feature input
    * @param result      the feature result to report
    * @param breadcrumbs names and references for linking back to parent reports
    * @param reportFiles the target report files (head = detail, tail = metas)
    */
  override def formatDetail(options: GwenOptions, unit: FeatureUnit, result: SpecResult, breadcrumbs: List[(String, File)], reportFiles: List[File]): Option[String] = {

    val reportDir = HtmlReportConfig.reportDir(options).get
    val featureName = result.spec.specFile.map(_.getPath()).getOrElse(result.spec.feature.name)
    val title = s"${result.spec.specType} Detail"
    val rootPath = relativePath(reportFiles.head, reportDir).filter(_ == File.separatorChar).flatMap(_ => "../")

    val htmlPage = 
      html(lang := "en",
        formatHtmlHead(s"$title - $featureName", rootPath),
        body(
          formatReportHeader(title, featureName, rootPath, this),
          DetailFormatter.formatDetailStatusBar(unit, result, rootPath, breadcrumbs, result.screenshots, true),
          formatDetailMetrics(result, result.summary),
          formatMetaResults(result.metaResults, reportFiles),
          for {
            scenario <- result.spec.scenarios
          } yield {
            formatScenario(scenario)
          },
          for {
            rule <- result.spec.rules
          } yield {
            formatRule(rule)
          }
        )
      )
    Some(Formatting.prettyPrintHTML("<!DOCTYPE html>" + htmlPage.render))

  }

  private def formatDetailMetrics(result: SpecResult, summary: ResultsSummary): TypedTag[String] = {
    val language = result.spec.feature.language
    val tags = result.spec.feature.tags
    div(`class` := "panel panel-default",
      div(`class` := "panel-heading", style := "padding-right: 20px; padding-bottom: 0px; border-style: none;",
        formatTags(tags),
        for {
          opt <- Option(language != "en")
          if opt
        } yield {
          span(`class` := "grayed",
            p(
              small(
                s"# language: $language"
              )
            )
          )
        },
        span(`class` := "label label-black",
          if (result.spec.specType.isFeature) {
            result.spec.feature.keyword
          } else {
            result.spec.specType.toString
          }
        ),
        raw(escapeHtml(result.spec.feature.name)),
        formatDescriptionLines(result.spec.feature.description, None),
        div(`class` := "panel-body", style := "padding-left: 0px; padding-right: 0px; margin-right: -10px;",
          span(`class` := "pull-right grayed", style := "padding-right: 10px;",
            small(
              s"Overhead: ${formatDuration(result.overhead)}"
            )
          ),
          table(width := "100%", attr("cellpadding") := "5",
            formatProgressBar(NodeType.Rule, summary.ruleCounts),
            formatProgressBar(NodeType.Scenario, summary.scenarioCounts),
            formatProgressBar(NodeType.Step, summary.stepCounts)
          )
        )
      )
    )
  }

  private def formatMetaResults(metaResults: List[SpecResult], reportFiles: List[File]): Option[TypedTag[String]] = {
    for {
      opt <- Option(metaResults.nonEmpty)
      if opt
    } yield {
      val count = metaResults.size
      val metaStatus = EvalStatus(metaResults.map(_.evalStatus))
      val status = metaStatus.keyword
      div(`class` := s"panel panel-${cssStatus(status)} bg-${cssStatus(status)}",
        ul(`class` := "list-group",
          li(`class` := s"list-group-item list-group-item-${cssStatus(status)}", style := "padding: 10px 10px; margin-right: 10px;",
            span(`class` := s"label label-${cssStatus(status)}",
              "Meta"
            ),
            a(`class` := s"text-${cssStatus(status)}", role := "button", attr("data-toggle") := "collapse", href := "#meta", attr("aria-expanded") := "true", attr("aria-controls") := "meta",
              s"$count meta feature${if (count > 1) "s" else ""}"
            ),
            span(`class` := "pull-right",
              small(
                formatDuration(DurationOps.sum(metaResults.map(_.elapsedTime)))
              )
            )
          )
        ),
        div(id := "meta", `class` :="panel-collapse collapse",
          div(`class` := "panel-body",
            ul(`class` := "list-group",
              li(`class` := s"list-group-item list-group-item-${cssStatus(status)}",
                div(`class` := "container-fluid", style := "padding: 0px 0px",
                  for {
                    (res, rowIndex) <- metaResults.zipWithIndex
                    reportPath = if (GwenSettings.`gwen.report.suppress.meta`) None else Some(s"meta/${reportFiles.tail(rowIndex).getName}")
                  } yield {
                    formatSummaryLine(res, reportPath, None, rowIndex)
                  }
                )
              )
            )
          )
        )
      )
    }
  }

  private def formatScenario(scenario: Scenario): TypedTag[String] = {
    val status = scenario.evalStatus.keyword
    val conflict = scenario.steps.map(_.evalStatus.keyword).exists(_ != status)
    val scenarioKeywordPixels = noOfKeywordPixels(scenario.steps)
    div(`class` := s"panel panel-${cssStatus(status)} bg-${cssStatus(status)}",
      for {
        background <- scenario.background
      } yield {
        formatBackground(scenario, background)
      },
      ul(`class` := "list-group",
        formatScenarioHeader(scenario)
      ),
      div(`class` := "panel-body",
        div(`class` := s"panel-${cssStatus(status)} ${if (conflict) s"bg-${cssStatus(status)}" else ""}", style := s"margin-bottom: 0px; ${if (conflict) "" else "border-style: none;"}",
          ul(`class` := "list-group",
            for {
              step <- scenario.steps
            } yield {
              Seq(
                if (!scenario.isOutline) {
                  Some(formatStepLine(step, step.evalStatus, scenarioKeywordPixels))
                } else if (!scenario.isExpanded) {
                  Some(formatRawStepLine(step, scenario.evalStatus, scenarioKeywordPixels))
                } else {
                  None
                }
              ).flatten
            }
          ),
          formatExamples(scenario.examples, scenarioKeywordPixels)
        )
      )
    )
  }

  private def formatScenarioHeader(scenario: Scenario): TypedTag[String] = {
    val status = scenario.evalStatus.keyword
    val tags = scenario.tags
    li(`class` := s"list-group-item list-group-item-${cssStatus(status)}", style := "padding: 10px 10px; margin-right: 10px;",
      for {
        opt <- Option(scenario.isStepDef)
        if opt
      } yield {
        span(`class` := "grayed",
          p(
            small(
              raw(
                scenario.sourceRef map { sref => s"${escapeHtml(sref.toString)}<br>" } mkString
              )
            )
          )
        )
      },
      formatTags(tags),
      span(`class` := s"label label-${cssStatus(status)}",
        if (scenario.isForEach) "ForEach" else scenario.keyword
      ),
      for {
        opt <- Option((scenario.steps.size + scenario.background.map(_.steps.size).getOrElse(0)) > 1 && !scenario.isForEach)
        if opt
      } yield {
        span(`class` := "pull-right",
          small(
            durationOrStatus(scenario.evalStatus).toString
          )
        )
      },
      raw(escapeHtml(scenario.name)),
      raw(" \u00a0 "),
      formatParams(scenario.params, status),
      if (!scenario.isForEach) {
        formatDescriptionLines(scenario.description, Some(status))
      } else {
        for {
          opt <- Option(scenario.steps.isEmpty)
          if opt
        } yield {
          span(`class` := "grayed",
            small(
              "-- none found --"
            )
          )
        }
      }
    )
  }

  private def formatBackground(parent: GherkinNode, background: Background): TypedTag[String] = {
    val status = background.evalStatus.keyword
    val keywordPixels = noOfKeywordPixels(background.steps)
    div(`class` := s"panel panel-${cssStatus(status)} bg-${cssStatus(status)}", style := "border-top: none; border-left:none; border-right: none; border-radius: 4px 4px 0 0",
      ul(`class` := "list-group",
        li(`class` := s"list-group-item list-group-item-${cssStatus(status)}", style := "padding: 10px 10px; margin-right: 10px;",
          span(`class` := s"label label-${cssStatus(status)}",
            background.keyword
          ),
          span(`class` := "pull-right",
            small(
              durationOrStatus(background.evalStatus).toString
            )
          ),
          if (!background.evalStatus.isFailed) {
            a(`class` := s"inverted inverted-${cssStatus(status)}", role := "button", attr("data-toggle") := "collapse", href := s"#${parent.uuid}-${background.uuid}", attr("aria-expanded") := "true", attr("aria-controls") := s"${parent.uuid}-${background.uuid}",
              raw(escapeHtml(background.name)),
            ),
          } else {
            raw(escapeHtml(background.name)),
          },
          div(id := s"${parent.uuid}-${background.uuid}", `class` := s"panel-collapse collapse${if (status != StatusKeyword.Passed) " in" else ""}", role := "tabpanel",
            formatDescriptionLines(background.description, Some(status)),
            div(`class` := "panel-body",
              ul(`class` := "list-group", style := "margin-right: -20px; margin-left: -10px; margin-top: 10px",
                for {
                  step <- background.steps
                } yield {
                  formatStepLine(step, step.evalStatus, keywordPixels)
                }
              )
            )
          )
        )
      )
    )
  }

  private def formatExamples(examples: List[Examples], keywordPixels: Int): Seq[TypedTag[String]] = {
    for {
      (exs, index) <- examples.zipWithIndex
      status = exs.evalStatus.keyword
    } yield {
      div(`class` := s"panel panel-${cssStatus(status)} bg-${cssStatus(status)}",
        ul(`class` := "list-group",
          li(`class` := s"list-group-item list-group-item-${cssStatus(status)}", style := "padding: 10px 10px; margin-right: 10px;",
            span(`class` := s"label label-${cssStatus(status)}",
              exs.keyword
            ),
            span(`class` := "pull-right",
              small(
                durationOrStatus(exs.evalStatus).toString
              )
            ),
            raw(escapeHtml(exs.name)),
            formatDescriptionLines(exs.description, Some(status))
          )
        ),
        div(`class` := "panel-body",
          ul(`class` := "list-group", style := "margin-right: -10px; margin-left: -10px",
            formatExampleHeader(exs.evalStatus, exs.table, keywordPixels),
            for {
              (scenario, subindex) <- exs.scenarios.zipWithIndex
            } yield {
              formatExampleRow(scenario, exs.table, subindex + 1, keywordPixels)
            }
          )
        )
      )
    }
  }

  private def formatExampleHeader(evalStatus: EvalStatus, table: List[(Long, List[String])], keywordPixels: Int): TypedTag[String] = {
    val status = evalStatus.keyword
    val line = table.head._1
    li(`class` := s"list-group-item list-group-item-${cssStatus(status)} ${if (evalStatus.isError) s"bg-${cssStatus(status)}" else ""}",
      div(`class` := s"bg-${cssStatus(status)}",
        div(`class` := "line-no",
          small(
            if (line > 0) line.toString else ""
          )
        ),
        div(`class` := "keyword-right", style := s"width:${keywordPixels}px",
          " ",
        ),
        formatDataRow(table, 0, status)
      )
    )
  }

  private def formatExampleRow(scenario: Scenario, table: List[(Long, List[String])], rowIndex: Int, keywordPixels: Int): TypedTag[String] = {
    val line = table(rowIndex)._1
    val status = scenario.evalStatus.keyword
    val rowHtml = formatDataRow(table, rowIndex, status)
    li(`class` := s"list-group-item list-group-item-${cssStatus(status)} ${if (scenario.evalStatus.isError) s"bg-${cssStatus(status)}" else ""}",
      div(`class` := s"bg-${cssStatus(status)}",
        span(`class` := "pull-right",
          small(
            durationOrStatus(scenario.evalStatus).toString
          )
        ),
        div(`class` := "line-no",
          small(
            if (line > 0) line.toString else ""
          )
        ),
        div(`class` := "keyword-right", style := s"width:${keywordPixels}px",
          " ",
        ),
        if (!scenario.evalStatus.isFailed) {
          a(`class` := s"inverted inverted-${cssStatus(status)}", role := "button", attr("data-toggle") := "collapse", href := s"#${scenario.uuid}", attr("aria-expanded") := "true", attr("aria-controls") := scenario.uuid,
            rowHtml
          )
        } else rowHtml,
        raw(" \u00a0 "),
        formatAttachments(scenario.attachments, status),
        formatExampleDiv(scenario, status)
      )
    )
  }

  private def formatExampleDiv(scenario: Scenario, status: StatusKeyword): TypedTag[String] = {
    div(id := scenario.uuid, `class` := s"panel-collapse collapse${if (status == StatusKeyword.Failed) " in" else ""}", role := "tabpanel",
      formatScenario(scenario)
    )
  }

  private def formatRule(rule: Rule): TypedTag[String] = {
    val status = rule.evalStatus.keyword
    val conflict = rule.scenarios.map(_.evalStatus.keyword).exists(_ != status)
    div(`class` := s"panel panel-${cssStatus(status)} bg-${cssStatus(status)}",
      for {
        background <- rule.background
      } yield {
        formatBackground(rule, background)
      },
      ul(`class` := "list-group",
        li(`class` := s"list-group-item list-group-item-${cssStatus(status)}", style := "padding: 10px 10px; margin-right: 10px;",
          span(`class` := s"label label-${cssStatus(status)}",
            rule.keyword
          ),
          for {
            opt <- Option(rule.evalScenarios.size > 1)
            if opt
          } yield {
            span(`class` := "pull-right",
              small(
                durationOrStatus(rule.evalStatus).toString
              )
            )
          },
          raw(escapeHtml(rule.name)),
          formatDescriptionLines(rule.description, Some(status))
        )
      ),
      div(`class` := "panel-body",
        div(`class` := s"panel-${cssStatus(status)} ${if (conflict) s"bg-${cssStatus(status)}" else ""}", style := s"margin-bottom: 0px; ${if (conflict) "" else "border-style: none;"}",
          ul(`class` := "list-group",
            for {
              scenario <- rule.scenarios
            } yield {
              formatScenario(scenario)
            }
          )
        )
      )
    )
  }

  private def formatStepLine(step: Step, evalStatus: EvalStatus, keywordPixels: Int): TypedTag[String] = {
    val status = evalStatus.keyword
    val stepDef = step.stepDef
    li(`class` := s"list-group-item list-group-item-${cssStatus(status)} ${if (evalStatus.isError || evalStatus.isDisabled) s"bg-${cssStatus(status)}" else ""}",
      div(`class` := s"bg-${cssStatus(status)} ${if (evalStatus.isDisabled) "text-muted" else ""}",
        span(`class` := "pull-right",
          small(
            durationOrStatus(step.evalStatus).toString
          )
        ),
        div(`class` := "line-no",
          small(
            step.sourceRef.map(_.line).getOrElse("").toString
          )
        ),
        div(`class` := "keyword-right", style := s"width:${keywordPixels}px",
          strong(
            step.keyword
          )
        ),
        " ",
        if (stepDef.nonEmpty && status == StatusKeyword.Passed) formatStepDefLink(step, status) else raw(escapeHtml(step.name)),
        raw(" \u00a0 "),
        formatParams(step.params, status),
        raw(" \u00a0 "),
        formatAttachments(step.deepAttachments, status),
        for {
          sd <- stepDef
          if (evalStatus.isEvaluated)  
        } yield {
          formatStepDefDiv(sd, status)
        },
        for {
          docString <- step.docString
        } yield {
          formatStepDocString(step, keywordPixels)
        },
        formatStepDataTable(step, keywordPixels)
      ),
      for {
        opt <- Option(evalStatus.isError && stepDef.isEmpty)
        if opt
      } yield {
        ul(
          li(`class` := s"list-group-item list-group-item-${cssStatus(status)} ${if (evalStatus.isError) s"bg-${cssStatus(status)}" else ""}",
            div(`class` := s"bg-${cssStatus(status)}",
              span(`class` := s"badge badge-${cssStatus(status)}${if(status != StatusKeyword.Passed && status != StatusKeyword.Loaded) s""" badge-${status.toString.toLowerCase}-issue""" else ""}",
                status.toString
              ),
              span(`class` := s"text-${cssStatus(status)}",
                small(
                  s" ${step.evalStatus.timestamp.toString} - ${step.evalStatus.message}"
                )
              )
            )
          )
        )
      }
    )
  }

  private def formatRawStepLine(step: Step, evalStatus: EvalStatus, keywordPixels: Int): TypedTag[String] = {
    val status = evalStatus.keyword
    li(`class` := s"list-group-item list-group-item-${cssStatus(status)} ${if (evalStatus.isError) s"bg-${cssStatus(status)}" else ""}",
      div(`class` := s"bg-${cssStatus(status)}",
        div(`class` := "line-no",
          small(
            step.sourceRef.map(_.line).getOrElse("").toString
          )
        ),
        div(`class` := "keyword-right", style := s"width:${keywordPixels}px",
          strong(
            step.keyword
          ),
          raw(escapeHtml(step.name))
        )
      )
    ) 
  }
  
  private def formatStepDefLink(step: Step, status: StatusKeyword): TypedTag[String] = {
    val stepDef = step.stepDef.get
    a(`class` := s"inverted inverted-${cssStatus(step.evalStatus.keyword)}", role := "button", attr("data-toggle") := "collapse", href := s"#${stepDef.uuid}", attr("aria-expanded") := "true", attr("aria-controls") := stepDef.uuid,
      raw(escapeHtml(step.name))
    )
  }
                  
  private def formatStepDefDiv(stepDef: Scenario, status: StatusKeyword): TypedTag[String] = {
    div(id := stepDef.uuid, `class` := s"panel-collapse collapse${if (status != StatusKeyword.Passed) " in" else ""}", role := "tabpanel",
      formatScenario(stepDef)
    )
  }

  private def formatStepDataTable(step: Step, keywordPixels: Int): Seq[TypedTag[String]] = {
    val status = step.evalStatus.keyword
    for {
      rowIndex <- step.table.indices
      line = step.table(rowIndex)._1
    } yield {
      div(`class` := s"bg-${cssStatus(status)}",
        div(`class` := "line-no",
          small(
            if (line > 0) line.toString else ""
          )
        ),
        div(`class` := "keyword-right", style := s"width:${keywordPixels}px",
          " "
        ),
        formatDataRow(step.table, rowIndex, status)
      )
    }
  }

  private def formatStepDocString(step: Step, keywordPixels: Int): Seq[TypedTag[String]] = {
    val status = step.evalStatus.keyword
    val docString = step.docString.get
    val contentType = docString._3
    for {
      (contentLine, index) <- formatDocString(docString, false).split("""\r?\n""").toList.zipWithIndex
      line = if (docString._1 > 0) docString._1 + index else 0
    } yield {
      div(`class` := s"bg-${cssStatus(status)}",
        div(`class` := "line-no",
          small(
            if (line > 0) line.toString else ""
          )
        ),
        div(`class` := "keyword-right", style := s"width:${keywordPixels}px",
          " "
        ),
        code(`class` := s"bg-${cssStatus(status)} doc-string",
          raw(escapeHtml(contentLine))
        ),
        for {
          cType <- contentType
          if (index == 0)
        } yield {
          code(`class` := s"bg-${cssStatus(status)} doc-string-type",
            cType
          )
        }
      )
    }
  }

  private def formatDataRow(table: List[(Long, List[String])], rowIndex: Int, status: StatusKeyword): TypedTag[String] = {
    code(`class` := s"bg-${cssStatus(status)} data-table",
      raw(escapeHtml(Formatting.formatTableRow(table, rowIndex)))
    )
  }
    
  private def formatAttachments(attachments: List[(String, File)], status: StatusKeyword): Option[TypedTag[String]] = {
    if (attachments.size > 1) {
      Some(
        div(`class` := s"dropdown bg-${cssStatus(status)}",
          button(`class` := s"btn btn-${cssStatus(status)} dropdown-toggle", attr("type") := "button", id := "dropdownMenu1", attr("data-toggle") := "dropdown", style := "vertical-align: text-top",
            strong(
              "attachments "
            ),
            span(`class` :="caret")
          ),
          ul(`class` := "dropdown-menu pull-right", role := "menu", style := "padding-left:0; max-width: 500px; width: max-content !important;",
            for {
              ((name, file), index) <- attachments.zipWithIndex
            } yield {
              li(role := "presentation", `class` := s"text-${cssStatus(status)}",
                a(role := "menuitem", tabindex := "-1", href := s"${attachmentHref(file)}", target := "_blank",
                  span(`class` := "line-no", style := "width: 0px;",
                    raw(s"${index + 1}. \u00a0 ")
                  ),
                  name,
                  span(`class` := "line-no", style := "width: 0px;",
                    raw(" \u00a0 ")
                  )
                )
              )
            }
          )
        )
      )
    } else if (attachments.size == 1)  {
      val (name, file) = attachments(0)
      Some(
        a(href := s"${attachmentHref(file)}", target := "_blank", style := s"color: ${linkColor(status)};",
          strong(style := "font-size: 12px;",
            name
          )
        )
      )
    } else {
      None
    }
  }

  def formatParams(params: List[(String, String)], status: StatusKeyword): Option[TypedTag[String]] = {
    if (params.size > 0) {
      Some(
        div(`class` := s"dropdown bg-${cssStatus(status)}",
          button(`class` := s"btn btn-${cssStatus(status)} dropdown-toggle", attr("type") := "button", id := "dropdownMenu1", attr("data-toggle") := "dropdown", style := "vertical-align: text-top",
            strong(
              "parameters "
            ),
            span(`class` :="caret")
          ),
          ul(`class` := "dropdown-menu pull-right", role := "menu", style := "padding-left:0; max-width: 500px; width: max-content !important;",
            li(role := "presentation", `class` := s"text-${cssStatus(status)}",
              table(width := "100%",
                tbody(`class` := "data-table",
                  for {
                    (name, value) <- params
                  } yield {
                    tr(
                      td(style := "padding: 3px; white-space: nowrap;", attr("align") := "right", 
                        span(`class` := "line-no",
                          s"$name :"
                        )
                      ),
                      td(style := "padding: 3px", 
                        raw(escapeHtml(value))
                      )
                    )
                  }
                )
              )
            )
          )
        )
      )
    } else {
      None
    }
  }

  private def attachmentHref(file: File) = if (FileIO.hasFileExtension("url", file)) Source.fromFile(file).mkString.trim else s"attachments/${file.getName}"
      
  private def formatTags(tags: List[gwen.core.node.gherkin.Tag]): Option[TypedTag[String]] = {
    if (tags.nonEmpty) {
      val tagsByLine = tags.groupBy(_.sourceRef.map(_.line).getOrElse(0L))
      val lines = tags.map(_.sourceRef.map(_.line).getOrElse(0L)).distinct.sorted
      Some(
        span(`class` := "grayed",
          p(
            small(
              raw(
                lines map { line => 
                  tagsByLine(line) map { tag => s"${escapeHtml(tag.toString)}" } mkString " "
                } mkString "<br>"
              )
            )
          )
        )
      )
    } else {
      None
    }
  }
  
  private def formatDescriptionLines(description: List[String], status: Option[StatusKeyword]): Option[Seq[TypedTag[String]]] = {
    val bgClass = status.map(cssStatus).getOrElse("default")
    if (description.nonEmpty) {
      Some(
        Seq(
          p,
          ul(`class` := s"list-group bg-$bgClass",
            for {
              line <- description
            } yield {
              li(`class` := s"list-group-item bg-$bgClass",
                raw(escapeHtml(line))
              )
            }
          )
        )
      )
    } else {
      None
    }
  }
  
  private def durationOrStatus(evalStatus: EvalStatus) =
    if (evalStatus.isEvaluated && !evalStatus.isDisabled)  {
      formatDuration(evalStatus.duration)
    } else {
      evalStatus.keyword
    }
  
  private def noOfKeywordPixels(steps: List[Step]): Int = steps match {
    case Nil => 9
    case _ => 
      val max = steps.map(_.keyword.length).max
      val factor = if (max < 4) 10 else 9
      max * factor
  }
        
}

object DetailFormatter { 

  import HtmlReportFormatter._

  private [format] def formatDetailStatusBar(unit: FeatureUnit, result: SpecResult, rootPath: String, breadcrumbs: List[(String, File)], screenshots: List[File], linkToError: Boolean): TypedTag[String] = {
    
    val status = result.evalStatus.keyword
    val sustainedCount = result.sustainedCount
    val renderErrorLink = linkToError && (status == StatusKeyword.Failed || sustainedCount > 0)

    div(
      formatElapsedStatus(result.elapsedTime),
      ol(`class` := "breadcrumb", style := "padding-right: 20px;",
        for {
          (text, reportFile) <- breadcrumbs
        } yield {
          li(
            span(`class` := "caret-left"),
            " ",
            a(href := s"${if (text == "Summary") rootPath else { if (result.isMeta) "../" else "" }}${reportFile.getName}",
              text
            )
          )
        },
        formatBadgeStatus(status, renderErrorLink, sustainedCount),
        formatDateStatus("Started", result.started),
        formatDateStatus("Finished", result.finished),
        Seq(
          if (GwenSettings.`gwen.report.slideshow.create` && screenshots.nonEmpty) {
            Some(
              li(
                SlideshowFormatter.formatSlideshowModal(screenshots, result.spec, unit, rootPath)
              )
            )
          } else None
        ).flatten
      )
    )

  }

}
