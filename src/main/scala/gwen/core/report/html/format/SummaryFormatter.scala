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
import gwen.core.GwenOptions
import gwen.core.node._
import gwen.core.report.ReportFormat
import gwen.core.report.html.HtmlReportConfig
import gwen.core.status._
import gwen.core.result.ResultsSummary

import scalatags.Text.all._
import scalatags.Text.TypedTag

import java.io.File

/** Formats the feature summary and detail reports in HTML. */
trait SummaryFormatter {
  this: HtmlReportFormatter =>
  
  import HtmlReportFormatter._
  
  /**
    * Formats the feature summary report as HTML.
    * 
    * @param options gwen command line options
    * @param info gwen info
    * @param summary the accumulated feature results summary
    */
  override def formatSummary(options: GwenOptions, info: GwenInfo, summary: ResultsSummary): Option[String] = {
    val title = "Results Summary"
    val path = if (options.args.isDefined) options.commandString else ""
    val htmlPage = 
      html(lang := "en",
        formatHtmlHead(title, "")
        ,body(
          formatReportHeader(title, path, "", info),
          formatSummaryStatusBar(summary),
          formatSummaryMetrics(summary),
          formatSummaryResults(options, summary)
        )
      )
    Some(Formatting.prettyPrintHTML("<!DOCTYPE html>" + htmlPage.render))
  }

  private def formatSummaryStatusBar(summary: ResultsSummary): TypedTag[String] = {
    
    val status = summary.evalStatus.keyword
    val sustainedCount = summary.sustainedCount

    div(
      formatElapsedStatus(summary.elapsedTime),
      ol(`class` := "breadcrumb", style := "padding-right: 20px;",
        li(style := "color: gray",
          span(`class` := "caret-left", style := "color: #f5f5f5;"),
          " Summary"
        ),
        formatBadgeStatus(status, false, sustainedCount),
        formatDateStatus("Started", summary.started),
        formatDateStatus("Finished", summary.finished),
      )
    )
  }

  private def formatSummaryMetrics(summary: ResultsSummary): TypedTag[String] = {
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
      results = summary.results.zipWithIndex filter { _._1.evalStatus.keyword == status }
      if results.nonEmpty
      count = results.size
      total = summary.results.size
      countOfTotal = s"""$count ${if (count != total) s" of $total features" else s"feature${if (total > 1) "s" else ""}"}"""
      summaryColWidhts = calcColWidths(summary.results, results.map(_._1))
    } yield {
      div(`class` := s"panel panel-${cssStatus(status)} bg-${bgStatus(status)}",
        ul(`class` := "list-group",
          li(`class` := s"list-group-item list-group-item-${bgStatus(status)}", style :="padding: 10px 10px; margin-right: 10px;",
            span(`class` := s"label label-${cssStatus(status)}",
              status.toString
            ),
            countOfTotal,
            for {
              opt <- Option(count > 1)
              if opt
            } yield {
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
            li(`class` := s"list-group-item list-group-item-${bgStatus(status)}", style := "padding-left:0px; padding-right:0px",
              table(`class` := "table table-responsive",
                tbody(
                  for {
                    ((result, resultIndex), rowIndex) <- results.zipWithIndex
                    reportFile = result.reports.get(ReportFormat.html).head
                  } yield {
                    formatSummaryLine(options, result, Some(s"${relativePath(reportFile, reportDir).replace(File.separatorChar, '/')}"), Some(resultIndex + 1), rowIndex, summaryColWidhts)
                  }
                )
              )
            )
          )
        )
      )
    }
  }
        
}
