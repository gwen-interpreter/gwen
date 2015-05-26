/*
 * Copyright 2014-2015 Branko Juric, Brady Wood
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
package gwen.report.html

import java.io.File
import java.text.DecimalFormat
import java.util.Date
import scala.concurrent.duration.Duration
import gwen.dsl.DurationFormatter
import gwen.dsl.EvalStatus
import gwen.dsl.Failed
import gwen.dsl.StatusKeyword
import gwen.dsl.Step
import gwen.eval.FeatureResult
import gwen.eval.FeatureSummary
import gwen.report.ReportFormatter
import gwen.GwenInfo
import gwen.eval.GwenOptions

/** Formats the feature summary and detail reports in HTML. */
trait HtmlReportFormatter extends ReportFormatter {
  
  private val cssStatus = Map(
      StatusKeyword.Passed -> "success", 
      StatusKeyword.Failed -> "danger", 
      StatusKeyword.Skipped -> "info", 
      StatusKeyword.Pending -> "warning",
      StatusKeyword.Loaded -> "success")
  
  private val percentFormatter = new DecimalFormat("#.##")

  /**
    * Formats the feature detail report as HTML.
    * 
    * @param info the gwen implementation info
    * @param result the feature result to report
    * @param breadcrumbs names and references for linking back to parent reports
    */
  override def formatDetail(info: GwenInfo, result: FeatureResult, breadcrumbs: List[(String, String)]): String = {
    
    val spec = result.spec
    val metaResults = result.metaResults 
    val featureName = spec.featureFile.map(_.getPath()).getOrElse(spec.feature.name)
    val scenarios = spec.scenarios
    val steps = spec.steps
    val isMeta = spec.featureFile.map(_.getName().endsWith(".meta")).getOrElse(false)
    
    val title = s"${if(isMeta) "Meta" else "Feature"} Detail"
    val status = spec.evalStatus.status
    
    s"""<!DOCTYPE html>
<html lang="en">
	<head>
		${formatHtmlHead(s"${title} - ${featureName}", "../")}
	</head>
	<body>
		${formatReportHeader(info, title, "../")}
		<ol class="breadcrumb">${(breadcrumbs map { case (text, report) => s"""
			<li>
				<span class="caret-left"></span> <a href="${report}">${escape(text)}</a>
			</li>"""}).mkString}
			<li>
				Evaluation Status
			</li>
			<li>
				<span class="badge badge-${cssStatus(status)}">${status}</span>
			</li>
			<li>
				<small>${escape(result.timestamp.toString)}</small>
			</li>
			<li>
				${escape(featureName)}
			</li>
		</ol>
		<div class="panel panel-default">
			<div class="panel-heading" style="padding-right: 20px; padding-bottom: 0px; border-style: none;">${if (spec.feature.tags.size > 0) s"""
					<span><p>${escape(spec.feature.tags.mkString(" "))}</p></span>""" else ""}
				<span class="label label-black">Feature:</span>
				<span class="pull-right"><small>${durationOrStatus(spec.evalStatus)}</small></span>
				${escape(spec.feature.name)}${if (!spec.feature.narrative.isEmpty) s"""
				<p>
				<ul class="list-group bg-default">${(spec.feature.narrative  map { line => 
					s"""<li class="list-group-item bg-default">${line}</li>"""}).mkString}
				</ul>
				</p>""" else ""}
				<div class="panel-body" style="padding-left: 0px; padding-right: 0px; margin-right: -10px;">
					<table width="100%" cellpadding="5">
						${formatProgressBar("Scenario", scenarios.map(_.evalStatus))}
						${formatProgressBar("Step", steps.map(_.evalStatus))}
					</table>
				</div>
			</div>
		</div>${if (!metaResults.isEmpty) { 
		val count = metaResults.size
		val metaStatus = EvalStatus(metaResults.map(_.evalStatus))
		val status = metaStatus.status
		s"""
		<div class="panel panel-${cssStatus(status)} bg-${cssStatus(status)}">
			<ul class="list-group">
				<li class="list-group-item list-group-item-${cssStatus(status)}" style="padding: 10px 10px; margin-right: 10px;">
					<span class="label label-${cssStatus(status)}">Meta:</span>
					${count} meta feature${if (count > 1) "s" else ""} ${if (count > 1) s"""
					<span class="pull-right"><small>${durationOrStatus(metaStatus)}</small></span>""" else ""}
				</li>
			</ul>
			<div class="panel-body">
				<ul class="list-group">
					<li class="list-group-item list-group-item-${cssStatus(status)}">
						<table width="100%">
							<tbody class="summary">${(metaResults map { result => formatSummaryLine(result, result.report.get.getName(), None)}).mkString}
							</tbody>
						</table>
					</li>
				</ul>
			</div>
		</div>"""} else ""}${(scenarios map { scenario => 
		val status = scenario.evalStatus.status
		val conflict = scenario.steps.map(_.evalStatus.status).exists(_ != status)
		s"""
		<div class="panel panel-${cssStatus(status)} bg-${cssStatus(status)}">
			<ul class="list-group">
				<li class="list-group-item list-group-item-${cssStatus(status)}" style="padding: 10px 10px; margin-right: 10px;">${if (scenario.tags.size > 0) s"""
					<span><p class="text-${cssStatus(status)}">${escape(scenario.tags.mkString(" "))}</p></span>""" else ""}
					<span class="label label-${cssStatus(status)}">Scenario:</span>${if (scenario.allSteps.size > 1) s"""
					<span class="pull-right"><small>${durationOrStatus(scenario.evalStatus)}</small></span>""" else ""}
					${escape(scenario.name)}
				</li>
			</ul>
			<div class="panel-body">${(scenario.background map { background => 
			    val status = background.evalStatus.status
			    s"""
				<div class="panel panel-${cssStatus(status)} bg-${cssStatus(status)}">
					<ul class="list-group">
						<li class="list-group-item list-group-item-${cssStatus(status)}" style="padding: 10px 10px;">
							<span class="label label-${cssStatus(status)}">Background:</span>${if (background.steps.size > 1) s"""
							<span class="pull-right"><small>${durationOrStatus(background.evalStatus)}</span></small>""" else ""}
							${escape(background.name)}
						</li>
					</ul>
					<div class="panel-body">
						<ul class="list-group" style="margin-right: -10px; margin-left: -10px">${(background.steps map { step => 
						    formatStepLine(step, step.evalStatus.status)}).mkString}
						</ul>
					</div>
				</div>"""}).getOrElse("")}
				<div class="panel-${cssStatus(status)} ${if (conflict) s"bg-${cssStatus(status)}" else ""}" style="margin-bottom: 0px; ${if (conflict) "" else "border-style: none;"}">
					<ul class="list-group">${(scenario.steps map { step => 
						formatStepLine(step, step.evalStatus.status)}).mkString}
					</ul>
				</div>
			</div>
		</div>"""}).mkString}${formatJsFooter("../")}
	</body>
</html>
"""
  }
  
  /**
    * Formats the feature summary report as HTML.
    * 
    * @param options gwen command line options
    * @param info the gwen implementation info
    * @param summary the accumulated feature results summary
    */
  override def formatSummary(options: GwenOptions, info: GwenInfo, summary: FeatureSummary): String = {
    
    val title = "Feature Summary";
    val status = EvalStatus(summary.featureResults.map(_.evalStatus)).status
  
    s"""<!DOCTYPE html>
<html lang="en">
	<head>
		${formatHtmlHead(title, "")}
	</head>
	<body>
		${formatReportHeader(info, title, "")}
		<ol class="breadcrumb">
			<li>
				Summary
			</li>
			<li>
				Evaluation Status
			</li>
			<li>
				<span class="badge badge-${cssStatus(status)}">${status}</span>
			</li>
			<li>
				<small>${escape(summary.timestamp.toString)}</small>
			</li>
		</ol>
		<div class="panel panel-default">
			<div class="panel-heading" style="padding-right: 20px; padding-bottom: 0px; border-style: none;">
				<span class="label label-black">Results</span>
				<span class="pull-right"><small>${formatDuration(summary.featureResults.map(_.evalStatus.duration).reduceLeft(_+_))}</small></span>
				<div class="panel-body" style="padding-left: 0px; padding-right: 0px; margin-right: -10px;">${if (options.args.isDefined) {s"""
					<p>
						<ul class="list-group bg-default"> 
							<li class="list-group-item bg-default">${escape(options.commandString(info))}</li>
						</ul>
					</p>"""} else ""}
					<table width="100%" cellpadding="5">
						${formatProgressBar("Feature", summary.featureResults.map(_.evalStatus))}
						${formatProgressBar("Scenario", summary.scenarioCounts)}
						${formatProgressBar("Step", summary.stepCounts)}
					</table>
				</div>
			</div>
		</div>${(StatusKeyword.valuesFixedOrder map { status => 
		summary.featureResults.filter { _.evalStatus.status == status } match {
		  case Nil => ""
		  case results => s"""
		<div class="panel panel-${cssStatus(status)} bg-${cssStatus(status)}">
			<ul class="list-group">
				<li class="list-group-item list-group-item-${cssStatus(status)}" style="padding: 10px 10px; margin-right: 10px;">
					<span class="label label-${cssStatus(status)}">${status}</span>${
					val count = results.size
					val total = summary.featureResults.size
					val countOfTotal = s"""${count} ${if (count != total) s" of ${total} features" else s"feature${if (total > 1) "s" else ""}"}"""
					s"""${countOfTotal}${if (count > 1) s"""
					<span class="pull-right"><small>${formatDuration(results.map(_.evalStatus.duration).reduceLeft(_+_))}</small></span>""" else ""}"""}
				</li>
			</ul>
			<div class="panel-body">
				<ul class="list-group">
					<li class="list-group-item list-group-item-${cssStatus(status)}">
						<table width="100%">
							<tbody class="summary">${
                (results.zipWithIndex map { case (result, index) => 
                  val report = result.report.get
                  formatSummaryLine(result, s"${report.getParentFile().getName()}/${report.getName()}", Some(index + 1))
                }).mkString}
							</tbody>
						</table>
					</li>
				</ul>
			</div>
		</div>"""}}).mkString}${formatJsFooter("")}
	</body>
</html>
    """
  }
  
  private def formatHtmlHead(title: String, rootDir: String) = s"""
		<meta charset="utf-8" />
		<meta http-equiv="X-UA-Compatible" content="IE=edge" />
		<meta name="viewport" content="width=device-width, initial-scale=1" />
		<title>${title}</title>
		<link href="${rootDir}resources/css/bootstrap.min.css" rel="stylesheet" />
		<link href="${rootDir}resources/css/gwen.css" rel="stylesheet" />"""
    
  private def formatReportHeader(info: GwenInfo, heading: String, rootDir: String) = s"""
		<table width="100%" cellpadding="5">
			<tr>
		  		<td width="100px">
					<a href="${info.gwenHome}"><img src="${rootDir}resources/img/gwen-logo.png" border="0" width="83px" height="115px"></img></a>
				</td>
				<td>
					<h3>${escape(heading)}</h3>
					Evaluation Report
				</td>
				<td align="right">
					<small>${escape(new Date().toString)}</small><h3></h3>
					<a href="${info.implHome}"><span class="badge" style="background-color: #1f23ae;">${escape(info.implName)}</span></a>
				</td>
			</tr>
		</table>"""

  private def formatProgressBar(name: String, evalStatuses: List[EvalStatus]): String = formatProgressBar(name, StatusKeyword.countsByStatus(evalStatuses))

  private def formatProgressBar(name: String, statusCounts: Map[StatusKeyword.Value, Int]): String = { 
    val total = (statusCounts map { case (_, count) => count }).sum
    s"""
						<tr>
    						<td align="right">
								<span style="white-space: nowrap;">${total} ${name}${if (total > 1) "s" else ""}</span>
							</td>
							<td width="99%">
								<div class="progress">${(StatusKeyword.valuesFixedOrder map { status =>
							  val count = statusCounts.get(status).getOrElse(0)
							  val percentage = calcPercentage(count, total)
							  s"""
								<div class="progress-bar progress-bar-${cssStatus(status)}" style="width: ${percentage}%">
									<span>${count} ${status} - ${percentageRounded(percentage)}%</span>
								</div>"""}).mkString}
							</div>
							</td>
						</tr>"""
  }
  
  private def formatSummaryLine(result: FeatureResult, reportPath: String, sequenceNo: Option[Int]): String = s"""
								<tr>${sequenceNo.map(seq => s"""
								    <td>
		  							    <div class="line-no"><small>${seq}</small></div>
									</td>""").getOrElse("")}
  									<td>
		  								<small>${escape(result.timestamp.toString)}</small>
  									</td>
									<td>
  										<a class="text-${cssStatus(result.evalStatus.status)}" href="${reportPath}">${escape(result.spec.feature.name)}</a>
  									</td>
									<td>
  										<span class="pull-right"><small>${durationOrStatus(result.evalStatus)}</small></span> ${result.spec.featureFile.map(_.getPath()).getOrElse("")}
  									</td>
								</tr>"""

  private def formatStepLine(step: Step, status: StatusKeyword.Value): String = s"""
							<li class="list-group-item list-group-item-${cssStatus(status)} ${if (status == StatusKeyword.Failed) s"bg-${cssStatus(status)}" else ""}">
								<div class="bg-${cssStatus(status)}">
									<span class="pull-right"><small>${durationOrStatus(step.evalStatus)}</small></span>
									<div class="line-no"><small>${step.pos.line}</small></div>
									<div class="keyword-right"><strong>${step.keyword}</strong></div> ${escape(step.expression)}
									${formatAttachments(step, status)}
								</div>
								${if (status == StatusKeyword.Failed) s"""
								<ul>
									<li class="list-group-item list-group-item-${cssStatus(status)} ${if (status == StatusKeyword.Failed) s"bg-${cssStatus(status)}" else ""}">
										<div class="bg-${cssStatus(status)}">
											<span class="badge badge-${cssStatus(status)}">${status}</span> <span class="text-${cssStatus(status)}"><small>${escape(step.evalStatus.asInstanceOf[Failed].timestamp.toString)} - ${escape(step.evalStatus.asInstanceOf[Failed].error.getCause().getMessage())}</small></span>
										</div>
									</li>
								</ul>""" else ""}
							</li>"""
									
  private def formatAttachments(step: Step, status: StatusKeyword.Value) = s"""
		  						${if (!step.attachments.isEmpty) s"""
								<div class="dropdown bg-${cssStatus(status)}">
		  							<button class="btn btn-${cssStatus(status)} dropdown-toggle" type="button" id="dropdownMenu1" data-toggle="dropdown">
		  								<strong>attachments</strong>
		  								<span class="caret"></span>
		  							</button>
		  							<ul class="dropdown-menu pull-right" role="menu">${(step.attachments map { case (name, file) => s"""
		  								<li role="presentation"><a role="menuitem" tabindex="-1" href="attachments/${file.getName()}" target="_blank">${escape(name)}</a></li>"""}).mkString }
		  							</ul>
		  						</div>""" else ""}"""

  private def formatJsFooter(rootDir: String) = s""" 
		<script src="${rootDir}resources/js/jquery-1.11.0.min.js"></script>
		<script src="${rootDir}resources/js/bootstrap.min.js"></script>"""
    
  private def percentageRounded(percentage: Double): String = percentFormatter.format(percentage)
  private def calcPercentage(count: Int, total: Int): Double = 100 * count.toDouble / total.toDouble
  private def durationOrStatus(evalStatus: EvalStatus) = evalStatus.status match {
    case StatusKeyword.Passed | StatusKeyword.Failed => formatDuration(evalStatus.duration)
    case _ => evalStatus.status
  }
  private def formatDuration(duration: Duration) = DurationFormatter.format(duration)
  private def escape(text: String) = String.valueOf(text).replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll("\"", "&quot;").replaceAll("'", "&#39;")
}