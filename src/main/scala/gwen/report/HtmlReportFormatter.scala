/*
 * Copyright 2014-2018 Branko Juric, Brady Wood
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
import java.text.DecimalFormat

import gwen.dsl._
import gwen.eval.FeatureResult
import gwen.eval.FeatureSummary
import gwen.{GwenInfo, GwenSettings}
import gwen.eval.GwenOptions
import gwen.report.ReportFormat.value2ReportFormat
import gwen.eval.FeatureUnit
import gwen.report.HtmlReportFormatter._
import gwen.Predefs.Formatting._
import gwen.Predefs.{DurationOps, Formatting}

import scala.util.Try

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
  override def formatDetail(options: GwenOptions, info: GwenInfo, unit: FeatureUnit, result: FeatureResult, breadcrumbs: List[(String, File)], reportFiles: List[File]): Option[String] = {

    val reportDir = ReportFormat.html.reportDir(options)
    val metaResults = result.metaResults
    val featureName = result.spec.featureFile.map(_.getPath()).getOrElse(result.spec.feature.name)
    val title = s"${if (result.isMeta) "Meta" else "Feature"} Detail"
    val summary = result.summary
    val screenshots = result.screenshots
    val rootPath = relativePath(reportFiles.head, reportDir).filter(_ == File.separatorChar).flatMap(_ => "../")

    Some(
      s"""<!DOCTYPE html>
<html lang="en">
  <head>
    ${formatHtmlHead(s"$title - $featureName", rootPath)}
    ${formatJsHeader(rootPath)}
  </head>
  <body>
    ${formatReportHeader(info, title, featureName, rootPath)}
    ${formatStatusHeader(unit, result, rootPath, breadcrumbs, screenshots)}
    <div class="panel panel-default">
      <div class="panel-heading" style="padding-right: 20px; padding-bottom: 0px; border-style: none;">${
        if (result.spec.feature.tags.nonEmpty)
          s"""
        <span class="grayed"><p><small>${result.spec.feature.tags.map(t => escapeHtml(t.toString)).mkString("<br>")}</small></p></span>""" else ""
      }
        <span class="label label-black">${if (result.isMeta) "Meta" else "Feature"}</span>
        ${escapeHtml(result.spec.feature.name)}${formatDescriptionLines(result.spec.feature.description, None)}
        <div class="panel-body" style="padding-left: 0px; padding-right: 0px; margin-right: -10px;">
          <span class="pull-right grayed" style="padding-right: 10px;"><small>Overhead: ${formatDuration(result.overhead)}</small></span>
          <table width="100%" cellpadding="5">
            ${formatProgressBar("Scenario", summary.scenarioCounts)}
            ${formatProgressBar("Step", summary.stepCounts)}
          </table>
        </div>
      </div>
    </div>${
        if (metaResults.nonEmpty) {
          val count = metaResults.size
          val metaStatus = EvalStatus(metaResults.map(_.evalStatus))
          val status = metaStatus.status
          s"""
    <div class="panel panel-${cssStatus(status)} bg-${cssStatus(status)}">
      <ul class="list-group">
        <li class="list-group-item list-group-item-${cssStatus(status)}" style="padding: 10px 10px; margin-right: 10px;">
          <span class="label label-${cssStatus(status)}">Meta</span>
          <a class="text-${cssStatus(status)}" role="button" data-toggle="collapse" href="#meta" aria-expanded="true" aria-controls="meta">
            $count meta feature${if (count > 1) "s" else ""}
          </a>
          <span class="pull-right"><small>${formatDuration(DurationOps.sum(metaResults.map(_.elapsedTime)))}</small></span>
        </li>
      </ul>  
      <div id="meta" class="panel-collapse collapse">
        <div class="panel-body">
          <ul class="list-group">
            <li class="list-group-item list-group-item-${cssStatus(status)}">
              <div class="container-fluid" style="padding: 0px 0px">
                ${(metaResults.zipWithIndex map { case (res, rowIndex) => formatSummaryLine(res, if (GwenSettings.`gwen.report.suppress.meta`) None else Some(s"meta/${reportFiles.tail(rowIndex).getName}"), None, rowIndex) }).mkString}
              </div>
            </li>
          </ul>
        </div>
      </div>
    </div>"""
        } else ""
      }${(result.spec.scenarios.zipWithIndex map { case (s, idx) => formatScenario(s, s"$idx") }).mkString}
  </body>
</html>
""")
  }

  private def formatDescriptionLines(description: List[String], status: Option[StatusKeyword.Value]) = {
    val bgClass = status.map(cssStatus).getOrElse("default")
    if (description.nonEmpty)
      s"""
        <p></p>
        <ul class="list-group bg-$bgClass">${
          (description map { line =>
            s"""<li class="list-group-item bg-$bgClass">${escapeHtml(line)}</li>"""
          }).mkString
        }
        </ul>"""
    else ""
  }

  private def formatScenario(scenario: Scenario, scenarioId: String): String = {
    val status = scenario.evalStatus.status
    val conflict = scenario.steps.map(_.evalStatus.status).exists(_ != status)
    val tags = scenario.tags.filter(t => t != Tag.StepDefTag && t != Tag.ForEachTag)
    s"""
    <a name="scenario-$scenarioId"></a><div class="panel panel-${cssStatus(status)} bg-${cssStatus(status)}">
      <ul class="list-group">
        <li class="list-group-item list-group-item-${cssStatus(status)}" style="padding: 10px 10px; margin-right: 10px;">${
      if (tags.nonEmpty)
        s"""
          <span class="grayed"><p><small>${tags.map(t => escapeHtml(t.toString)).mkString("<br>")}</small></p></span>""" else ""
    }
          <span class="label label-${cssStatus(status)}">${scenario.keyword}</span>${
      if ((scenario.steps.size + scenario.background.map(_.steps.size).getOrElse(0)) > 1)
        s"""
          <span class="pull-right"><small>${durationOrStatus(scenario.evalStatus)}</small></span>""" else ""
    }
          ${escapeHtml(scenario.name)}${if (!scenario.isForEach) s"${formatDescriptionLines(scenario.description, Some(status))}" else { if(scenario.steps.isEmpty) """ <span class="grayed"><small>-- none found --</small></span>""" else ""}}
        </li>
      </ul>
      <div class="panel-body">${
      (scenario.background map { background =>
        val status = background.evalStatus.status
        val backgroundId = s"$scenarioId-background"
        s"""
        <div class="panel panel-${cssStatus(status)} bg-${cssStatus(status)}">
          <ul class="list-group">
            <li class="list-group-item list-group-item-${cssStatus(status)}" style="padding: 10px 10px;">
              <span class="label label-${cssStatus(status)}">${Background.keyword}</span>
              <span class="pull-right"><small>${durationOrStatus(background.evalStatus)}</span></small>
              ${escapeHtml(background.name)}${formatDescriptionLines(background.description, Some(status))}
            </li>
          </ul>
          <div class="panel-body">
            <ul class="list-group" style="margin-right: -10px; margin-left: -10px">${
          (background.steps.zipWithIndex map { case (step, index) =>
            formatStepLine(step, step.evalStatus.status, s"$backgroundId-${step.pos.line}-${index + 1}")
          }).mkString
        }
            </ul>
          </div>
        </div>"""
      }).getOrElse("")
    }
        <div class="panel-${cssStatus(status)} ${if (conflict) s"bg-${cssStatus(status)}" else ""}" style="margin-bottom: 0px; ${if (conflict) "" else "border-style: none;"}">
          <ul class="list-group">${
          (scenario.steps.zipWithIndex map { case (step, index) =>
            if (!scenario.isOutline) {
              formatStepLine(step, step.evalStatus.status, s"$scenarioId-${step.pos.line}-${index + 1}")
            } else {
              formatRawStepLine(step, scenario.evalStatus.status)
            }
          }).mkString}
          </ul>
        ${if (scenario.isOutline) formatExamples(scenario.examples, scenarioId) else ""}
        </div>
      </div>
    </div>"""
  }

  private def formatExamples(examples: List[Examples], outlineId: String): String = (examples.zipWithIndex map { case (exs, index) =>
    val exampleId = s"$outlineId-examples-${index}"
    val status = exs.evalStatus.status
    s"""
       <p></p>
       <div class="panel panel-${cssStatus(status)} bg-${cssStatus(status)}">
         <ul class="list-group">
           <li class="list-group-item list-group-item-${cssStatus(status)}" style="padding: 10px 10px; margin-right: 10px;">
             <span class="label label-${cssStatus(status)}">${Examples.keyword}</span>
             <span class="pull-right"><small>${durationOrStatus(exs.evalStatus)}</small></span>
             ${escapeHtml(exs.name)}${formatDescriptionLines(exs.description, Some(status))}
           </li>
         </ul>
        <div class="panel-body">
          <ul class="list-group" style="margin-right: -10px; margin-left: -10px">${
            formatExampleHeader(exs.evalStatus, exs.table)}${
            (exs.scenarios.zipWithIndex map { case (scenario, subindex) =>
              formatExampleRow(scenario, exs.table, subindex + 1, s"$exampleId-${subindex}")
            }).mkString
          }
          </ul>
        </div>
      </div>"""
  }).mkString

  private def formatExampleHeader(evalStatus: EvalStatus, table: List[(Int, List[String])]): String = {
              val status = evalStatus.status
              val line = table.head._1
              s"""
                <li class="list-group-item list-group-item-${cssStatus(status)} ${if (EvalStatus.isError(status)) s"bg-${cssStatus(status)}" else ""}">
                <div class="bg-${cssStatus(status)}">
                  <div class="line-no"><small>${if (line > 0) line else ""}</small></div>
                  <div class="keyword-right"> </div>${formatDataRow(table, 0, status)}
                </div>
              </li>"""
  }
  private def formatExampleRow(scenario: Scenario, table: List[(Int, List[String])], rowIndex: Int, exampleId: String): String = {
              val line = table(rowIndex)._1
              val status = scenario.evalStatus.status
              val rowHtml = formatDataRow(table, rowIndex, status)
              s"""
                <li class="list-group-item list-group-item-${cssStatus(status)} ${if (EvalStatus.isError(status)) s"bg-${cssStatus(status)}" else ""}">
                <div class="bg-${cssStatus(status)}">
                  <span class="pull-right"><small>${durationOrStatus(scenario.evalStatus)}</small></span>
                  <div class="line-no"><small>${if (line > 0) line else ""}</small></div>
                  <div class="keyword-right"> </div>${if (status == StatusKeyword.Passed) formatExampleLink(rowHtml, status, s"$exampleId") else rowHtml }
                  ${formatAttachments(scenario.attachments, status)} ${if (EvalStatus.isEvaluated(status)) formatExampleDiv(scenario, status, exampleId) else ""}
                </div>
              </li>"""
  }

  private def formatExampleLink(rowHtml: String, status: StatusKeyword.Value, exampleId: String): String =
                  s"""<a class="inverted inverted-${cssStatus(status)}" role="button" data-toggle="collapse" href="#$exampleId" aria-expanded="true" aria-controls="$exampleId">${rowHtml}</a>"""

  private def formatExampleDiv(scenario: Scenario, status: StatusKeyword.Value, exampleId: String): String = s"""
                  <div id="$exampleId" class="panel-collapse collapse${if (status != StatusKeyword.Passed) " in" else ""}" role="tabpanel">
                  ${formatScenario(scenario, exampleId)}
                  </div>"""

  private def formatStepDataTable(step: Step): String = {
      val status = step.evalStatus.status
              s"""
              ${step.table.indices map { rowIndex =>
                val line = step.table(rowIndex)._1
                val row = Formatting.formatTableRow(step.table, rowIndex)
              s"""
                <div class="bg-${cssStatus(status)}">
                  <div class="line-no"><small>${if (line > 0) line else ""}</small></div>
                  <div class="keyword-right"> </div>${formatDataRow(step.table, rowIndex, status)}
                </div>"""} mkString}"""
  }

  private def formatStepDocString(step: Step): String = {
      val status = step.evalStatus.status
      val docString = step.docString.get
      val contentType = docString._3
      s"""
              ${formatDocString(docString, false).split("""\r?\n""").zipWithIndex  map { case (contentLine, index) =>
                val line = docString._1 + index
              s"""
                <div class="bg-${cssStatus(status)}">
                  <div class="line-no"><small>${if (line > 0) line else ""}</small></div>
                  <div class="keyword-right"> </div><code class="bg-${cssStatus(status)} doc-string">${escapeHtml(contentLine).replaceAll("  ", " &nbsp;")}</code>${if (index == 0) contentType.map(cType => s"""<code class="bg-${cssStatus(status)} doc-string-type">${escapeHtml(cType)}</code>""").getOrElse("") else ""}
                </div>"""} mkString}"""
  }

  private def formatDataRow(table: List[(Int, List[String])], rowIndex: Int, status: StatusKeyword.Value): String = {
    s"""<code class="bg-${cssStatus(status)} data-table">${escapeHtml(Formatting.formatTableRow(table, rowIndex)).replaceAll("  ", " &nbsp;")}</code>"""
  }
  
  /**
    * Formats the feature summary report as HTML.
    * 
    * @param options gwen command line options
    * @param info the gwen implementation info
    * @param summary the accumulated feature results summary
    */
  override def formatSummary(options: GwenOptions, info: GwenInfo, summary: FeatureSummary): Option[String] = {
    
    val reportDir = ReportFormat.html.reportDir(options)
    val title = "Feature Summary"
    val status = summary.evalStatus.status
    val sustainedCount = summary.sustainedCount
  
    Some(s"""<!DOCTYPE html>
<html lang="en">
  <head>
    ${formatHtmlHead(title, "")}
  </head>
  <body>
    ${formatReportHeader(info, title, if (options.args.isDefined) escapeHtml(options.commandString(info)) else "", "")}
    
    <ol class="breadcrumb" style="padding-right: 20px;">
      <li style="color: gray">
        <span class="caret-left" style="color: #f5f5f5;"></span> Summary
      </li>
      <li>
        <span class="badge badge-${cssStatus(status)}">$status</span>
        ${if (sustainedCount > 0) s""" <small><span class="grayed">with</span></small> <span class="badge badge-danger">${sustainedCount} sustained error${if (sustainedCount > 1) "s" else ""}</span>""" else ""}
      </li>
      <li>
        <small><span class="grayed">Started: </span>${escapeHtml(summary.started.toString)}</small>
      </li>
      <li>
        <small><span class="grayed">Finished: </span>${escapeHtml(summary.finished.toString)}</small>
      </li>
      <span class="pull-right"><small>${formatDuration(summary.elapsedTime)}</small></span>
    </ol>
    <div class="panel panel-default">
      <div class="panel-heading" style="padding-right: 20px; padding-bottom: 0px; border-style: none;">
        <span class="label label-black">Results</span>
        <div class="panel-body" style="padding-left: 0px; padding-right: 0px; margin-right: -10px;">
          <span class="pull-right grayed" style="padding-right: 10px;"><small>Overhead: ${formatDuration(summary.overhead)}</small></span>
          <table width="100%" cellpadding="5">
            ${formatProgressBar("Feature", summary.featureCounts)}
            ${formatProgressBar("Scenario", summary.scenarioCounts)}
            ${formatProgressBar("Step", summary.stepCounts)}
          </table>
        </div>
      </div>
    </div>${(StatusKeyword.reportables.reverse map { status => 
    summary.results.zipWithIndex.filter { _._1.evalStatus.status == status } match {
      case Nil => ""
      case results => s"""
    <div class="panel panel-${cssStatus(status)} bg-${cssStatus(status)}">
      <ul class="list-group">
        <li class="list-group-item list-group-item-${cssStatus(status)}" style="padding: 10px 10px; margin-right: 10px;">
          <span class="label label-${cssStatus(status)}">$status</span>${
          val count = results.size
          val total = summary.results.size
          val countOfTotal = s"""$count ${if (count != total) s" of $total features" else s"feature${if (total > 1) "s" else ""}"}"""
          s"""$countOfTotal${if (count > 1) s"""
          <span class="pull-right"><small>${formatDuration(DurationOps.sum(results.map(_._1.elapsedTime)))}</small></span>""" else ""}"""}
        </li>
      </ul>
      <div class="panel-body">
        <ul class="list-group">
          <li class="list-group-item list-group-item-${cssStatus(status)}">
            <div class="container-fluid" style="padding: 0px 0px">${
                (results.zipWithIndex map { case ((result, resultIndex), rowIndex) => 
                  val reportFile = result.reports.get(ReportFormat.html).head
                  formatSummaryLine(result, Some(s"${relativePath(reportFile, reportDir).replace(File.separatorChar, '/')}"), Some(resultIndex + 1), rowIndex)
                }).mkString}
            </div>
          </li>
        </ul>
      </div>
    </div>"""}}).mkString}
  </body>
</html>
    """)
  }
  
  private def formatHtmlHead(title: String, rootPath: String) = s"""
    <meta charset="utf-8" />
    <meta http-equiv="X-UA-Compatible" content="IE=edge" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>$title</title>
    <link href="${rootPath}resources/css/bootstrap.min.css" rel="stylesheet" />
    <link href="${rootPath}resources/css/gwen.css" rel="stylesheet" />"""
    
  private def formatProgressBar(name: String, counts: Map[StatusKeyword.Value, Int]): String = { 
    val total = counts.values.sum
    if (total > 0) {s"""
            <tr>
              <td align="right">
                <span style="white-space: nowrap;">$total $name${if (total > 1) "s" else ""}</span>
              </td>
              <td width="99%">
                <div class="progress">${(StatusKeyword.reportables map { status =>
                val count = counts.getOrElse(status, 0)
                val percentage = calcPercentage(count, total)
                s"""
                <div class="progress-bar progress-bar-${cssStatus(status)}" style="width: $percentage%">
                  <span>$count $status - ${percentageRounded(percentage)}%</span>
                </div>"""}).mkString}
              </div>
              </td>
            </tr>"""} else ""
  }
  
  private def formatSummaryLine(result: FeatureResult, reportPath: Option[String], sequenceNo: Option[Int], rowIndex: Int): String = {
    val featureName = Option(result.spec.feature.name).map(_.trim).filter(!_.isEmpty).getOrElse(result.spec.featureFile.map(_.getName()).map(n => Try(n.substring(0, n.lastIndexOf('.'))).getOrElse(n)).getOrElse("-- details --"))
    val reportingStatus = result.evalStatus match {
      case Passed(nanos) if result.sustainedCount > 0 => Sustained(nanos, null)
      case status => status
    }
    s"""
                <div class="row${if (rowIndex % 2 == 1) s" bg-altrow-${cssStatus(result.evalStatus.status)}" else "" }">
                  <div class="col-md-3" style="padding-left: 0px">${sequenceNo.map(seq => s"""
                    <div class="line-no"><small>$seq</small></div>""").getOrElse("")}
                    <span style="padding-left: 15px; white-space: nowrap;"><small>${escapeHtml(result.finished.toString)}</small></span>
                  </div>
                  <div class="col-md-4">${reportPath.fold(s"${escapeHtml(featureName)}") { rpath =>
                    s"""<a class="text-${cssStatus(reportingStatus.status)}" style="color: ${linkColor(reportingStatus.status)};" href="$rpath"><span class="text-${cssStatus(reportingStatus.status)}">${escapeHtml(featureName)}</span></a>"""}}
                  </div>
                  <div class="col-md-5">
                    <span class="pull-right"><small>${formatDuration(result.elapsedTime)}</small></span> ${result.spec.featureFile.map(_.getPath()).getOrElse("")}
                  </div>
                </div>"""
  }
  private def formatStepLine(step: Step, status: StatusKeyword.Value, stepId: String): String = s"""
        <li class="list-group-item list-group-item-${cssStatus(status)} ${if (EvalStatus.isError(status)) s"bg-${cssStatus(status)}" else ""}">
                <div class="bg-${cssStatus(status)}">
                  <span class="pull-right"><small>${durationOrStatus(step.evalStatus)}</small></span>
                  <div class="line-no"><small>${if (step.pos.line > 0) step.pos.line else ""}</small></div>
                  <div class="keyword-right"><strong>${step.keyword}</strong></div> ${if (step.stepDef.isDefined && status == StatusKeyword.Passed) formatStepDefLink(step, status, s"$stepId-stepDef") else s"${escapeHtml(step.name)}"}
                  ${formatAttachments(step.attachments, status)} ${step.stepDef.map{ stepDef => if (EvalStatus.isEvaluated(status)) { formatStepDefDiv(stepDef, status, s"$stepId-stepDef") } else ""}.getOrElse("")}${if (step.docString.nonEmpty) formatStepDocString(step) else if (step.table.nonEmpty) formatStepDataTable(step) else ""}
                </div>
                ${if (EvalStatus.isError(status) && step.stepDef.isEmpty) s"""
                <ul>
                  <li class="list-group-item list-group-item-${cssStatus(status)} ${if (EvalStatus.isError(status)) s"bg-${cssStatus(status)}" else ""}">
                    <div class="bg-${cssStatus(status)}">
                      <span class="badge badge-${cssStatus(status)}${if(status != StatusKeyword.Passed && status != StatusKeyword.Loaded) s""" badge-${status.toString.toLowerCase}-issue""" else ""}">$status</span> <span class="text-${cssStatus(status)}"><small>${escapeHtml(step.evalStatus.timestamp.toString)} - ${escapeHtml(step.evalStatus.cause.get.getMessage)}</small></span>
                    </div>
                  </li>
                </ul>""" else ""}
              </li>"""

  private def formatRawStepLine(step: Step, status: StatusKeyword.Value): String = s"""
              <li class="list-group-item list-group-item-${cssStatus(status)} ${if (EvalStatus.isError(status)) s"bg-${cssStatus(status)}" else ""}">
                <div class="bg-${cssStatus(status)}">
                  <div class="line-no"><small>${if (step.pos.line > 0) step.pos.line else ""}</small></div>
                  <div class="keyword-right"><strong>${step.keyword}</strong></div> ${escapeHtml(step.name)}
                </div>
              </li>"""
  
  private def formatStepDefLink(step: Step, status: StatusKeyword.Value, stepDefId: String): String = 
    s"""<a class="inverted inverted-${cssStatus(step.evalStatus.status)}" role="button" data-toggle="collapse" href="#$stepDefId" aria-expanded="true" aria-controls="$stepDefId">${escapeHtml(step.name)}</a>"""
                  
  private def formatStepDefDiv(stepDef: Scenario, status: StatusKeyword.Value, stepDefId: String): String = s"""
                  <div id="$stepDefId" class="panel-collapse collapse${if (status != StatusKeyword.Passed) " in" else ""}" role="tabpanel" ${if (stepDef.metaFile.isEmpty) """style="padding-left: 40px;"""" else ""}>
                  ${formatScenario(stepDef, stepDefId)}
                  </div>"""
    
  private def formatAttachments(attachments: List[(String, File)], status: StatusKeyword.Value) = s"""
                  ${if (attachments.nonEmpty) s"""
                  <div class="dropdown bg-${cssStatus(status)}">
                    <button class="btn btn-${cssStatus(status)} dropdown-toggle" type="button" id="dropdownMenu1" data-toggle="dropdown">
                      <strong>attachment${if (attachments.size > 1) "s" else ""}</strong>
                      <span class="caret"></span>
                    </button>
                    <ul class="dropdown-menu pull-right" role="menu">${(attachments map { case (name, file) =>
                    val number = file.getName.substring(0, file.getName.indexOf("-")).toInt
                    s"""
                      <li role="presentation" class="text-${cssStatus(status)}"><a role="menuitem" tabindex="-1" href="attachments/${file.getName}" target="_blank"><span class="line-no" style="width: 0px;">${number}. &nbsp; </span>${escapeHtml(name)}</a></li>"""}).mkString }
                    </ul>
                  </div>""" else ""}"""

  private def formatJsHeader(rootPath: String) = s""" 
    <script src="${rootPath}resources/js/jquery.min.js"></script>
    <script src="${rootPath}resources/js/bootstrap.min.js"></script>"""
      
  private def percentageRounded(percentage: Double): String = percentFormatter.format(percentage)
  private def calcPercentage(count: Int, total: Int): Double = 100 * count.toDouble / total.toDouble
  private def durationOrStatus(evalStatus: EvalStatus) =
    if (EvalStatus.isEvaluated(evalStatus.status))  {
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
    StatusKeyword.Loaded -> "success")

  private val linkColor = Map(
    StatusKeyword.Passed -> "#3c763d",
    StatusKeyword.Failed -> "#a94442",
    StatusKeyword.Sustained -> "#a94442",
    StatusKeyword.Skipped -> "#8a6d3b",
    StatusKeyword.Pending -> "#31708f",
    StatusKeyword.Loaded -> "#3c763d"
  )
  
  private [report] def formatReportHeader(info: GwenInfo, heading: String, path: String, rootPath: String) = s"""
    <table width="100%" cellpadding="5">
      <tr>
        <td width="100px">
          <a href="${info.gwenHome}"><img src="${rootPath}resources/img/gwen-logo.png" border="0" width="83px" height="115px"></img></a>
        </td>
        <td>
          <h3>${escapeHtml(heading)}</h3>
          ${escapeHtml(path)}
        </td>
        <td align="right">
          <h3>&nbsp;</h3>
          <a href="${info.implHome}"><span class="badge" style="background-color: #1f23ae;">${escapeHtml(info.implName)}</span></a>
          <p><small style="white-space: nowrap; color: #1f23ae; padding-right: 7px;">${info.releaseNotesUrl.map(url => s"""<a href="$url">""").getOrElse("")}v${escapeHtml(info.implVersion)}${info.releaseNotesUrl.map(_ => "</a>").getOrElse("")}</small>
          </p>
        </td>
      </tr>
    </table>"""
         
  private [report] def formatStatusHeader(unit: FeatureUnit, result: FeatureResult, rootPath: String, breadcrumbs: List[(String, File)], screenshots: List[File]) = {
    val status = result.evalStatus.status
    val renderStatusLink = status != StatusKeyword.Passed && status != StatusKeyword.Loaded
    val sustainedCount = result.sustainedCount
    s"""
    <ol class="breadcrumb" style="padding-right: 20px;">${(breadcrumbs map { case (text, reportFile) => s"""
      <li>
        <span class="caret-left"></span> <a href="${if (text == "Summary") rootPath else { if (result.isMeta) "../" else "" }}${reportFile.getName}">${escapeHtml(text)}</a>
      </li>"""}).mkString}
      <li>
        <span class="badge badge-${cssStatus(status)}">${if (renderStatusLink) s"""<a id="error-issue" href="#" style="color:white;">""" else ""}${status}${if (renderStatusLink) """</a><script>$(document).ready(function(){$('#error-issue').click(function(e){e.preventDefault();$('html, body').animate({scrollTop:$('.badge-error-issue').closest('.panel').offset().top},500);});});</script>""" else ""}</span>
        ${if (sustainedCount > 0) s""" <small><span class="grayed">with</span></small> <span class="badge badge-danger"><a id="sustained-issue" href="#" style="color:white;">${sustainedCount} sustained error${if (sustainedCount > 1) "s" else ""}</a><script>$$(document).ready(function(){$$('#sustained-issue').click(function(e){e.preventDefault();$$('html, body').animate({scrollTop:$$('.badge-sustained-issue').closest('.panel').offset().top},500);});});</script></span>""" else ""}
      </li>
      <li>
        <small><span class="grayed">Started: </span>${escapeHtml(result.started.toString)}</small>
      </li>
      <li>
        <small><span class="grayed">Finished: </span>${escapeHtml(result.finished.toString)}</small>
      </li>
        ${ if (GwenSettings.`gwen.report.slideshow.create` && screenshots.nonEmpty) { s"""
             <li>
               ${formatSlideshow(screenshots, result.spec, unit, rootPath)} 
             </li>"""
           } else ""
         }
      <span class="pull-right"><small>${formatDuration(result.elapsedTime)}</small></span>
    </ol>"""
  }

  private def formatSlideshow(screenshots: List[File], spec: FeatureSpec, unit: FeatureUnit, rootPath: String) = s"""
  <div class="modal fade" id="slideshow" tabindex="-1" role="dialog" aria-labelledby="slideshowLabel" aria-hidden="true">
  <div class="modal-dialog" style="width: 60%;">
  <div class="modal-content">
    <div class="modal-body">
    <a href="${ReportFormat.slideshow.getReportDetailFilename(spec, unit.dataRecord)}.${ReportFormat.slideshow.fileExtension}" id="full-screen">Full Screen</a>
    <a href="#" title="Close"><span id="close-btn" class="pull-right glyphicon glyphicon-remove-circle" aria-hidden="true"></span></a>
    ${HtmlSlideshowFormatter.formatSlideshow(screenshots, rootPath)}
   </div>
  </div>
  </div>
  </div>
  <button type="button" class="btn btn-default btn-lg" data-toggle="modal" data-target="#slideshow">
    Slideshow
  </button>
  <script>
    $$('#close-btn').click(function(e) { e.preventDefault(); $$('#slideshow').modal('hide'); });
    $$('#full-screen').click(function(e) { $$('#close-btn').click(); });
    $$('#slideshow').on('show.bs.modal', function (e) { $$('#slides').reel('frame', 1); stop(); });
    $$('#slideshow').on('hide.bs.modal', function (e) { $$('#slides').trigger('stop') });
    $$('#slideshow').on('hidden.bs.modal', function (e) { $$('#slides').trigger('stop') });
  </script>
  """
          
}