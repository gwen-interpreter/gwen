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
package gwen.core.report.html

import HtmlReportFormatter._

import gwen.core._
import gwen.core.Formatting._
import gwen.core.GwenOptions
import gwen.core.model._
import gwen.core.model.gherkin._
import gwen.core.report.ReportFormat
import gwen.core.report.ReportFormatter

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Try
import scalatags.Text.all._
import scalatags.Text.TypedTag

import java.io.File
import java.text.DecimalFormat
import java.util.Date

/** Formats the feature summary and detail reports in HTML. */
trait HtmlReportFormatter extends ReportFormatter {

  private val percentFormatter = new DecimalFormat("#.##")

  /**
    * Formats the feature detail report as HTML.
    *
    * @param options     gwen command line options
    * @param info        the gwen implementation info
    * @param unit        the feature input
    * @param result      the feature result to report
    * @param breadcrumbs names and references for linking back to parent reports
    * @param reportFiles the target report files (head = detail, tail = metas)
    */
  override def formatDetail(options: GwenOptions, info: GwenInfo, unit: FeatureUnit, result: SpecResult, breadcrumbs: List[(String, File)], reportFiles: List[File]): Option[String] = {

    val reportDir = HtmlReportConfig.reportDir(options).get
    val featureName = result.spec.specFile.map(_.getPath()).getOrElse(result.spec.feature.name)
    val title = s"${result.spec.specType} Detail"
    val rootPath = relativePath(reportFiles.head, reportDir).filter(_ == File.separatorChar).flatMap(_ => "../")

    val htmlPage = 
      html(lang := "en",
        formatHtmlHead(s"$title - $featureName", rootPath),
        body(
          formatReportHeader(info, title, featureName, rootPath),
          formatDetailStatusHeader(unit, result, rootPath, breadcrumbs, result.screenshots, true),
          formatDetailHeader(result, result.summary),
          formatMetaResults(result.metaResults, reportFiles),
          for (scenario <- result.spec.scenarios) yield formatScenario(scenario),
          for (rule <- result.spec.rules) yield formatRule(rule)
        )
      )
    Some(Formatting.prettyPrintHTML("<!DOCTYPE html>" + htmlPage.render))

  }

  private def formatDetailHeader(result: SpecResult, summary: ResultsSummary): TypedTag[String] = {
    val language = result.spec.feature.language
    val tags = result.spec.feature.tags
    div(`class` := "panel panel-default",
      div(`class` := "panel-heading", style := "padding-right: 20px; padding-bottom: 0px; border-style: none;",
        formatTags(tags),
        if (language != "en") {
          span(`class` := "grayed",
            p(
              small(
                s"# language: $language"
              )
            )
          )
        },
        span(`class` := "label label-black",
          result.spec.specType.toString
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
    if (metaResults.nonEmpty) {
      val count = metaResults.size
      val metaStatus = EvalStatus(metaResults.map(_.evalStatus))
      val status = metaStatus.status
      Some(
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
                    } yield
                    formatSummaryLine(res, reportPath, None, rowIndex)
                  )
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

  private def formatDescriptionLines(description: List[String], status: Option[StatusKeyword.Value]): Option[Seq[TypedTag[String]]] = {
    val bgClass = status.map(cssStatus).getOrElse("default")
    if (description.nonEmpty) {
      Some(
        Seq(
          p,
          ul(`class` := s"list-group bg-$bgClass",
            for (line <- description)
            yield
            li(`class` := s"list-group-item bg-$bgClass",
              raw(escapeHtml(line))
            )
          )
        )
      )
    } else {
      None
    }
  }

  private def formatScenario(scenario: Scenario): TypedTag[String] = {
    val status = scenario.evalStatus.status
    val conflict = scenario.steps.map(_.evalStatus.status).exists(_ != status)
    val scenarioKeywordPixels = noOfKeywordPixels(scenario.steps)
    div(`class` := s"panel panel-${cssStatus(status)} bg-${cssStatus(status)}",
      ul(`class` := "list-group",
        formatScenarioHeader(scenario)
      ),
      div(`class` := "panel-body",
        for {
          background <- scenario.background
        } yield
        formatBackground(background),
        div(`class` := s"panel-${cssStatus(status)} ${if (conflict) s"bg-${cssStatus(status)}" else ""}", style := s"margin-bottom: 0px; ${if (conflict) "" else "border-style: none;"}",
          ul(`class` := "list-group",
            for {
              step <- scenario.steps
            } yield
            Seq(
              if (!scenario.isOutline) {
                Some(formatStepLine(step, step.evalStatus.status, scenarioKeywordPixels))
              } else if (!scenario.isExpanded) {
                Some(formatRawStepLine(step, scenario.evalStatus.status, scenarioKeywordPixels))
              } else {
                None
              }
            ).flatten
          ),
          if (scenario.isOutline) {
            formatExamples(scenario.examples, scenarioKeywordPixels)
          }
        )
      )
    )
  }

  private def formatScenarioHeader(scenario: Scenario): TypedTag[String] = {
    val status = scenario.evalStatus.status
    val tags = scenario.tags
    li(`class` := s"list-group-item list-group-item-${cssStatus(status)}", style := "padding: 10px 10px; margin-right: 10px;",
      if (scenario.isStepDef) {
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
      if ((scenario.steps.size + scenario.background.map(_.steps.size).getOrElse(0)) > 1 && !scenario.isForEach) {
        span(`class` := "pull-right",
          small(
            durationOrStatus(scenario.evalStatus).toString
          )
        )
      },
      raw(escapeHtml(scenario.name)),
      if (!scenario.isForEach) {
        formatDescriptionLines(scenario.description, Some(status))
      } else if (scenario.steps.isEmpty) {
        span(`class` := "grayed",
          small(
            "-- none found --"
          )
        )
      }
    )
  }

  private def formatTags(tags: List[model.Tag]): Option[TypedTag[String]] = {
    if (tags.nonEmpty) {
      Some(
        span(`class` := "grayed",
          p(
            small(
              raw(
                tags map { tag => s"${escapeHtml(tag.toString)}<br>" } mkString
              )
            )
          )
        )
      )
    } else {
      None
    }
  }

  private def formatBackground(background: Background): TypedTag[String] = {
    val status = background.evalStatus.status
    val keywordPixels = noOfKeywordPixels(background.steps)
    div(`class` := s"panel panel-${cssStatus(status)} bg-${cssStatus(status)}",
      ul(`class` := "list-group",
        li(`class` := s"list-group-item list-group-item-${cssStatus(status)}", style := "padding: 10px 10px;",
          span(`class` := s"label label-${cssStatus(status)}",
            background.keyword
          ),
          span(`class` := "pull-right",
            small(
              durationOrStatus(background.evalStatus).toString
            )
          ),
          raw(escapeHtml(background.name)),
          formatDescriptionLines(background.description, Some(status))
        )
      ),
      div(`class` := "panel-body",
        ul(`class` := "list-group", style := "margin-right: -10px; margin-left: -10px",
        for (step <- background.steps)
        yield
        formatStepLine(step, step.evalStatus.status, keywordPixels)
        )
      )
    )
  }

  private def formatExamples(examples: List[Examples], keywordPixels: Int): Seq[TypedTag[String]] = {
    for {
      (exs, index) <- examples.zipWithIndex
      status = exs.evalStatus.status
    } yield
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
          for ((scenario, subindex) <- exs.scenarios.zipWithIndex)
          yield
          formatExampleRow(scenario, exs.table, subindex + 1, keywordPixels)
        )
      )
    )
  }

  private def formatExampleHeader(evalStatus: EvalStatus, table: List[(Int, List[String])], keywordPixels: Int): TypedTag[String] = {
    val status = evalStatus.status
    val line = table.head._1
    li(`class` := s"list-group-item list-group-item-${cssStatus(status)} ${if (EvalStatus.isError(status)) s"bg-${cssStatus(status)}" else ""}",
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

  private def formatExampleRow(scenario: Scenario, table: List[(Int, List[String])], rowIndex: Int, keywordPixels: Int): TypedTag[String] = {
    val line = table(rowIndex)._1
    val status = scenario.evalStatus.status
    val rowHtml = formatDataRow(table, rowIndex, status)
    li(`class` := s"list-group-item list-group-item-${cssStatus(status)} ${if (EvalStatus.isError(status)) s"bg-${cssStatus(status)}" else ""}",
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
        if (status != StatusKeyword.Failed) {
          a(`class` := s"inverted inverted-${cssStatus(status)}", role := "button", attr("data-toggle") := "collapse", href := s"#${scenario.uuid}", attr("aria-expanded") := "true", attr("aria-controls") := scenario.uuid,
            rowHtml
          )
        } else rowHtml,
        " ",
        formatAttachments(scenario.attachments, status),
        formatExampleDiv(scenario, status)
      )
    )
  }

  private def formatExampleDiv(scenario: Scenario, status: StatusKeyword.Value): TypedTag[String] = {
    div(id := scenario.uuid, `class` := s"panel-collapse collapse${if (status == StatusKeyword.Failed) " in" else ""}", role := "tabpanel",
      formatScenario(scenario)
    )
  }

  private def formatStepDataTable(step: Step, keywordPixels: Int): Seq[TypedTag[String]] = {
    val status = step.evalStatus.status
    for {
      rowIndex <- step.table.indices
      line = step.table(rowIndex)._1
    } yield
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

  private def formatStepDocString(step: Step, keywordPixels: Int): Seq[TypedTag[String]] = {
    val status = step.evalStatus.status
    val docString = step.docString.get
    val contentType = docString._3
    for {
      (contentLine, index) <- formatDocString(docString, false).split("""\r?\n""").toList.zipWithIndex
      line = docString._1 + index
    } yield
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
      } yield
      code(`class` := s"bg-${cssStatus(status)} doc-string-type",
        cType
      )
    )
  }

  private def formatDataRow(table: List[(Int, List[String])], rowIndex: Int, status: StatusKeyword.Value): TypedTag[String] = {
    code(`class` := s"bg-${cssStatus(status)} data-table",
      raw(escapeHtml(Formatting.formatTableRow(table, rowIndex)))
    )
  }

  private def formatRule(rule: Rule): TypedTag[String] = {
    val status = rule.evalStatus.status
    val conflict = rule.scenarios.map(_.evalStatus.status).exists(_ != status)
    div(`class` := s"panel panel-${cssStatus(status)} bg-${cssStatus(status)}",
      ul(`class` := "list-group",
        li(`class` := s"list-group-item list-group-item-${cssStatus(status)}", style := "padding: 10px 10px; margin-right: 10px;",
          span(`class` := s"label label-${cssStatus(status)}",
            rule.keyword
          ),
          if (rule.evalScenarios.size > 1) {
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
        for {
          background <- rule.background
        } yield
        formatBackground(background),
        div(`class` := s"panel-${cssStatus(status)} ${if (conflict) s"bg-${cssStatus(status)}" else ""}", style := s"margin-bottom: 0px; ${if (conflict) "" else "border-style: none;"}",
          ul(`class` := "list-group",
            for (scenario <- rule.scenarios)
            yield
            formatScenario(scenario)
          )
        )
      )
    )
  }
  
  /**
    * Formats the feature summary report as HTML.
    * 
    * @param options gwen command line options
    * @param info the gwen implementation info
    * @param summary the accumulated feature results summary
    */
  override def formatSummary(options: GwenOptions, info: GwenInfo, summary: ResultsSummary): Option[String] = {
    val title = "Feature Summary"
    val path = if (options.args.isDefined) escapeHtml(options.commandString(info)) else ""
    val htmlPage = 
      html(lang := "en",
        formatHtmlHead(title, "")
        ,body(
          formatReportHeader(info, title, path, ""),
          formatSummaryStatusHeader(summary),
          formatSummaryHeader(summary),
          formatSummaryResults(options, summary)
        )
      )
    Some(Formatting.prettyPrintHTML("<!DOCTYPE html>" + htmlPage.render))
  }

  private def formatSummaryHeader(summary: ResultsSummary): TypedTag[String] = {
    div(`class` := "panel panel-default",
      div(`class` := "panel-heading", style := "padding-right: 20px; padding-bottom: 0px; border-style: none;",
        span(`class` := "label label-black",
          "Results"
        ),
        div(`class` := "panel-body", style := "padding-left: 0px; padding-right: 0px; margin-right: -10px;",
          span(`class` := "pull-right grayed", style := "padding-right: 10px;",
            small(
              s"Overhead: ${formatDuration(summary.overhead)}"
            )
          ),
          table(width := "100%", attr("cellpadding") := "5",
            formatProgressBar(NodeType.Feature, summary.featureCounts),
            formatProgressBar(NodeType.Rule, summary.ruleCounts),
            formatProgressBar(NodeType.Scenario, summary.scenarioCounts),
            formatProgressBar(NodeType.Step, summary.stepCounts)
          )
        )
      )
    )
  }

  private def formatSummaryResults(options: GwenOptions, summary: ResultsSummary): Seq[TypedTag[String]] = {
    val reportDir = HtmlReportConfig.reportDir(options).get
    for {
      status <- StatusKeyword.reportables.reverse
      results = summary.results.zipWithIndex.filter { _._1.evalStatus.status == status }
      (result, index) <- results
      count = results.size
      total = summary.results.size
      countOfTotal = s"""$count ${if (count != total) s" of $total features" else s"feature${if (total > 1) "s" else ""}"}"""
    } yield
    div(`class` := s"panel panel-${cssStatus(status)} bg-${cssStatus(status)}",
      ul(`class` := "list-group",
        li(`class` := s"list-group-item list-group-item-${cssStatus(status)}", style :="padding: 10px 10px; margin-right: 10px;",
          span(`class` := s"label label-${cssStatus(status)}",
            status.toString
          ),
          countOfTotal,
          if (count > 1) {
            span(`class` := "pull-right",
              small(
                formatDuration(DurationOps.sum(results.map(_._1.elapsedTime)))
              )
            )
          }
        )
      ),
      div(`class` := "panel-body",
        ul(`class` := "list-group",
          li(`class` := s"list-group-item list-group-item-${cssStatus(status)}",
            div(`class` := "container-fluid", style := "padding: 0px 0px",
             for {
               ((result, resultIndex), rowIndex) <- results.zipWithIndex
               reportFile = result.reports.get(ReportFormat.html).head
             } yield
             formatSummaryLine(result, Some(s"${relativePath(reportFile, reportDir).replace(File.separatorChar, '/')}"), Some(resultIndex + 1), rowIndex)
            )
          )
        )
      )
    )
  }

  private def formatProgressBar(nodeType: NodeType.Value, counts: Map[StatusKeyword.Value, Int]): Option[TypedTag[String]] = { 
    for (total <- Some(counts.values.sum).filter(_ > 0))
    yield
    tr(
      td(attr("align") := "right",
        span(style := "white-space: nowrap;",
          s"$total $nodeType${if (total > 1) "s" else ""}"
        )
      ),
      td(width := "99%",
        div(`class` := "progress",
          for { 
            status <- StatusKeyword.reportables
            count = counts.getOrElse(status, 0)
            percentage = calcPercentage(count, total)
          } yield
          div(`class` := s"progress-bar progress-bar-${cssStatus(status)}", style := s"width: $percentage%;",
            span(
              s"$count $status - ${percentageRounded(percentage)}%"
            )
          )
        )
      )
    )
  }
  
  private def formatSummaryLine(result: SpecResult, reportPath: Option[String], sequenceNo: Option[Int], rowIndex: Int): TypedTag[String] = {
    val featureName = Option(result.spec.feature.name).map(_.trim).filter(!_.isEmpty).getOrElse(result.spec.specFile.map(_.getName()).map(n => Try(n.substring(0, n.lastIndexOf('.'))).getOrElse(n)).getOrElse("-- details --"))
    val reportingStatus = result.evalStatus match {
      case Passed(nanos) if result.sustainedCount > 0 => Sustained(nanos, null)
      case status => status
    }
    div(`class` := s"row${if (rowIndex % 2 == 1) s" bg-altrow-${cssStatus(result.evalStatus.status)}" else "" }",
      div(`class` := "col-md-3", style := "padding-left: 0px",
        for (seq <- sequenceNo)
        yield
        div(`class` := "line-no",
          small(
            seq.toString
          )
        ),
        span(style := "padding-left: 15px; white-space: nowrap;",
          small(
            result.finished.toString
          )
        )
      ),
      div(`class` := "col-md-4",
        reportPath match {
          case Some(rpath) =>
            a(`class` := s"text-${cssStatus(reportingStatus.status)}", style := s"color: ${linkColor(reportingStatus.status)};", href := rpath,
              span(`class` := s"text-${cssStatus(reportingStatus.status)}",
                raw(escapeHtml(featureName))
              )
            )
          case None =>
            raw(escapeHtml(featureName))
        }
      ),
      div(`class` := "col-md-5",
        span(`class` := "pull-right",
          small(
            formatDuration(result.elapsedTime)
          )
        ),
        result.spec.specFile.map(_.getPath()).getOrElse("").toString
      )
    )
  }
  
  private def formatStepLine(step: Step, status: StatusKeyword.Value, keywordPixels: Int): TypedTag[String] = {
    val stepDef = step.stepDef
    li(`class` := s"list-group-item list-group-item-${cssStatus(status)} ${if (EvalStatus.isError(status) || EvalStatus.isDisabled(status)) s"bg-${cssStatus(status)}" else ""}",
      div(`class` := s"bg-${cssStatus(status)} ${if (EvalStatus.isDisabled(status)) "text-muted" else ""}",
        span(`class` := "pull-right",
          small(
            durationOrStatus(step.evalStatus).toString
          )
        ),
        div(`class` := "line-no",
          small(
            step.sourceRef.map(_.pos.line).getOrElse("").toString
          )
        ),
        div(`class` := "keyword-right", style := s"width:${keywordPixels}px",
          strong(
            step.keyword
          )
        ),
        " ",
        if (stepDef.nonEmpty && status == StatusKeyword.Passed) formatStepDefLink(step, status) else raw(escapeHtml(step.name)),
        " ",
        formatAttachments(step.deepAttachments, status),
        for {
          (sd, _) <- stepDef
          if (EvalStatus.isEvaluated(status))  
        } yield
        formatStepDefDiv(sd, status),
        for (docString <- step.docString)
        yield
        formatStepDocString(step, keywordPixels),
        formatStepDataTable(step, keywordPixels)
      ),
      if (EvalStatus.isError(status) && stepDef.isEmpty) {
        ul(
          li(`class` := s"list-group-item list-group-item-${cssStatus(status)} ${if (EvalStatus.isError(status)) s"bg-${cssStatus(status)}" else ""}",
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

  private def formatRawStepLine(step: Step, status: StatusKeyword.Value, keywordPixels: Int): TypedTag[String] = {
    li(`class` := s"list-group-item list-group-item-${cssStatus(status)} ${if (EvalStatus.isError(status)) s"bg-${cssStatus(status)}" else ""}",
      div(`class` := s"bg-${cssStatus(status)}",
        div(`class` := "line-no",
          small(
            step.sourceRef.map(_.pos.line).getOrElse("").toString
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
  
  private def formatStepDefLink(step: Step, status: StatusKeyword.Value): TypedTag[String] = {
    val stepDef = step.stepDef.get._1
    a(`class` := s"inverted inverted-${cssStatus(step.evalStatus.status)}", role := "button", attr("data-toggle") := "collapse", href := s"#${stepDef.uuid}", attr("aria-expanded") := "true", attr("aria-controls") := stepDef.uuid,
      raw(escapeHtml(step.name))
    )
  }
                  
  private def formatStepDefDiv(stepDef: Scenario, status: StatusKeyword.Value): TypedTag[String] = {
    div(id := stepDef.uuid, `class` := s"panel-collapse collapse${if (status != StatusKeyword.Passed) " in" else ""}", role := "tabpanel",
      formatScenario(stepDef)
    )
  }
    
  private def formatAttachments(attachments: List[(String, File)], status: StatusKeyword.Value): Option[TypedTag[String]] = {
    if (attachments.size > 1) {
      Some(
        div(`class` := s"dropdown bg-${cssStatus(status)}",
          button(`class` := s"btn btn-${cssStatus(status)} dropdown-toggle", attr("type") := "button", id := "dropdownMenu1", attr("data-toggle") := "dropdown", style := "vertical-align: text-top",
            strong(
              "attachments "
            ),
            span(`class` :="caret")
          ),
          ul(`class` := "dropdown-menu pull-right", role := "menu", style := "padding-left:0;",
            for (((name, file), index) <- attachments.zipWithIndex)
            yield
            li(role := "presentation", `class` := s"text-${cssStatus(status)}",
              a(role := "menuitem", tabindex := "-1", href := s"${attachmentHref(file)}", target := "_blank",
                span(`class` := "line-no", style := "width: 0px;",
                  raw(s"${index + 1}. &nbsp; ")
                ),
                name,
                span(`class` := "line-no", style := "width: 0px;",
                  raw(" &nbsp; ")
                )
              )
            )
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

  private def attachmentHref(file: File) = if (FileIO.hasFileExtension("url", file)) Source.fromFile(file).mkString.trim else s"attachments/${file.getName}"
      
  private def percentageRounded(percentage: Double): String = percentFormatter.format(percentage)
  private def calcPercentage(count: Int, total: Int): Double = 100 * count.toDouble / total.toDouble
  private def durationOrStatus(evalStatus: EvalStatus) =
    if (EvalStatus.isEvaluated(evalStatus.status) && !EvalStatus.isDisabled(evalStatus.status))  {
      formatDuration(evalStatus.duration)
    } else {
      evalStatus.status
    }
  
}

object HtmlReportFormatter {
  
  private val cssStatus = Map(
    StatusKeyword.Passed -> "success", 
    StatusKeyword.Failed -> "danger",
    StatusKeyword.Sustained -> "danger",
    StatusKeyword.Skipped -> "warning",
    StatusKeyword.Pending -> "info",
    StatusKeyword.Loaded -> "success",
    StatusKeyword.Disabled -> "default")

  private val linkColor = Map(
    StatusKeyword.Passed -> "#3c763d",
    StatusKeyword.Failed -> "#a94442",
    StatusKeyword.Sustained -> "#a94442",
    StatusKeyword.Skipped -> "#8a6d3b",
    StatusKeyword.Pending -> "#31708f",
    StatusKeyword.Loaded -> "#3c763d",
    StatusKeyword.Disabled -> "grey"
  )

  private [report] def formatHtmlHead(pageTitle: String, rootPath: String): TypedTag[String] = {
    head(
      meta(charset := "utf-8"),
      meta(httpEquiv := "X-UA-Compatible", content := "IE=edge"),
      meta(name := "viewport", content := "width=device-width, initial-scale=1"),
      title := pageTitle,
      link(href := s"${rootPath}resources/css/bootstrap.min.css", rel := "stylesheet"),
      link(href := s"${rootPath}resources/css/gwen.css", rel := "stylesheet"),
      script(src := s"${rootPath}resources/js/jquery.min.js"),
      script(src := s"${rootPath}resources/js/bootstrap.min.js")
    )
  }
  
  private [report] def formatReportHeader(info: GwenInfo, heading: String, path: String, rootPath: String): TypedTag[String] = {
    val implVersion = s"v${info.implVersion}"
    table(width := "100%", attr("cellpadding") := "5",
      tr(
        td(width := "100px",
          a(href := info.gwenHome,
            img(src := s"${rootPath}resources/img/gwen-logo.png", border := "0", width := "82px")
          )
        ),
        td(
          h3(
            raw(escapeHtml(heading))
          ),
          path
        ),
        td(attr("align") := "right",
          h3(" "),
          a(href := info.implHome,
            span(`class` := "badge", style := "background-color: #1f23ae;", 
              info.implName
            )
          ),
          p(
            small(style := "white-space: nowrap; color: #1f23ae; padding-right: 7px;",
              info.releaseNotesUrl map { url => a(href := url, implVersion) } getOrElse implVersion
            )
          )
        )
      )
    )
  }
         
  private [report] def formatSummaryStatusHeader(summary: ResultsSummary): TypedTag[String] = {
    
    val status = summary.evalStatus.status
    val sustainedCount = summary.sustainedCount

    ol(`class` := "breadcrumb", style := "padding-right: 20px;",
      li(style := "color: gray",
        span(`class` := "caret-left", style := "color: #f5f5f5;"),
        " Summary"
      ),
      formatBadgeStatus(status, false, sustainedCount),
      formatDateStatus("Started", summary.started),
      formatDateStatus("Finished", summary.finished),
      formatElapsedStatus(summary.elapsedTime)
    )
  }

  private [report] def formatDetailStatusHeader(unit: FeatureUnit, result: SpecResult, rootPath: String, breadcrumbs: List[(String, File)], screenshots: List[File], linkToError: Boolean): TypedTag[String] = {
    
    val status = result.evalStatus.status
    val sustainedCount = result.sustainedCount
    val renderErrorLink = linkToError && (status == StatusKeyword.Failed || sustainedCount > 0)

    ol(`class` := "breadcrumb", style := "padding-right: 20px;",
      for ((text, reportFile) <- breadcrumbs) 
      yield
      li(
        span(`class` := "caret-left"),
        " ",
        a(href := s"${if (text == "Summary") rootPath else { if (result.isMeta) "../" else "" }}${reportFile.getName}",
          text
        )
      ),
      formatBadgeStatus(status, renderErrorLink, sustainedCount),
      formatDateStatus("Started", result.started),
      formatDateStatus("Finished", result.finished),
      if (GwenSettings.`gwen.report.slideshow.create` && screenshots.nonEmpty) {
        li(
          formatSlideshow(screenshots, result.spec, unit, rootPath)
        )
      },
      formatElapsedStatus(result.elapsedTime)
    )

  }

  private def formatBadgeStatus(status: StatusKeyword.Value, renderErrorLink: Boolean, sustainedCount: Int): TypedTag[String] = {
    val sustainedError = s"${sustainedCount} sustained error${if (sustainedCount > 1) "s" else ""}"
    li(
      span(`class` := s"badge badge-${cssStatus(status)}",
        if (renderErrorLink && status == StatusKeyword.Failed) {
          Seq(
            a(id := "failed-link", href := "#", style := "color:white;",
              status.toString
            ),
            script(
              formatFailedLinkScript("failed")
            )
          )
        } else {
          status.toString
        }
      ),
      if (sustainedCount > 0) {
        small(
          span(`class` := "grayed", 
            " with "
          ),
          span(`class` := "badge badge-danger",
            if (renderErrorLink) {
              Seq(
                a(id := "sustained-link", href := "#", style := "color:white;",
                  sustainedError
                ),
                script(
                  formatFailedLinkScript("sustained")
                )
              )
            } else {
              sustainedError
            }
          )
        )
      }
    )
  }

  private def formatDateStatus(label: String, date: Date): TypedTag[String] = {
    li(
      small(
        span(`class` := "grayed", s"$label: "),
        date.toString
      )
    )
  }

  private def formatElapsedStatus(elapsedTime: Duration): TypedTag[String] = {
    span(`class` := "pull-right",
      small(formatDuration(elapsedTime))
    )
  }

  private def formatFailedLinkScript(statusType: String): String = {
    s"""|$$(document).ready(function() {
        |  $$('#${statusType}-link').click(
        |    function(e) {
        |      e.preventDefault();
        |      $$('html, body').animate({scrollTop:$$('.badge-${statusType}-issue').closest('.panel').offset().top}, 500);
        |    }
        |  );
        |});""".stripMargin
  }

  private def formatSlideshow(screenshots: List[File], spec: Spec, unit: FeatureUnit, rootPath: String): Seq[TypedTag[String]] = {
    Seq(
      div(`class` := "modal fade", id := "slideshow", tabindex := "-1", role := "dialog", attr("aria-labelledby") := "slideshowLabel", attr("aria-hidden") := "true",
        div(`class` := "modal-dialog", style := "width: 60%;",
          div(`class` := "modal-content",
            div(`class` := "modal-body",
              a(href := s"${HtmlSlideshowConfig.getReportDetailFilename(spec, unit.dataRecord).get}.${HtmlSlideshowConfig.fileExtension.get}", id := "full-screen",
                "Full Screen"
              ),
              a(href := "#", title := "Close",
                span(id :="close-btn", `class` := "pull-right glyphicon glyphicon-remove-circle", attr("aria-hidden") := "true")
              ),
              HtmlSlideshowFormatter.formatSlideshow(screenshots, rootPath)
            )
          )
        )
      ),
      button(attr("type") := "button", `class` := "btn btn-default btn-lg", attr("data-toggle") := "modal", attr("data-target") := "#slideshow",
        "Slideshow"
      ),
      script(
        raw(
          """|
             |          $('#close-btn').click(function(e) { e.preventDefault(); $('#slideshow').modal('hide'); });
             |          $('#full-screen').click(function(e) { $('#close-btn').click(); });
             |          $('#slideshow').on('show.bs.modal', function (e) { $('#slides').reel('frame', 1); stop(); });
             |          $('#slideshow').on('hide.bs.modal', function (e) { $('#slides').trigger('stop') });
             |          $('#slideshow').on('hidden.bs.modal', function (e) { $('#slides').trigger('stop') });
             |""".stripMargin
        )
      )
    )
  }

  private def noOfKeywordPixels(steps: List[Step]): Int = steps match {
    case Nil => 9
    case _ => 
      val max = steps.map(_.keyword.length).max
      val factor = if (max < 4) 10 else 9
      max * factor
  }
          
}