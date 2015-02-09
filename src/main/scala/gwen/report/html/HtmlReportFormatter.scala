/*
 * Copyright 2014 Branko Juric, Brady Wood
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

import java.text.DecimalFormat
import java.util.Date
import gwen.dsl.EvalStatus
import gwen.dsl.Failed
import gwen.dsl.FeatureSpec
import gwen.dsl.Step
import gwen.report.FeatureSummary
import gwen.report.ReportFormatter
import gwen.dsl.StatusKeyword
import java.io.File
import gwen.report.FeatureResult

/**
 * Formats the feature summary and detail reports in HTML.
 */
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
   * @param feature
   * 			the feature to report
   * @param interpreterName
   * 			the gwen interpreter name
   * @param backlinks
   *   			names and references for linking back to parent reports
   * @param metaReportFiles
   *   			list of meta report files (if any)
   */
  override def formatDetail(spec: FeatureSpec, interpreterName: String, backlinks: List[(String, File)], metaReportFiles: List[File] = Nil): String = {
    
    val metaResults = spec.metaSpecs zip metaReportFiles map { case(meta, metaReport) => FeatureResult(meta, Some(metaReport)) }
    val metas = spec.metaSpecs zip metaResults
    val featureName = spec.featureFile.map(_.getPath()).getOrElse(spec.feature.name)
    val scenarios = spec.scenarios
    val steps = spec.steps
    val isMeta = spec.featureFile.map(_.getName().endsWith(".meta")).getOrElse(false)
    
    val title = s"${if(isMeta) "Meta" else "Feature"} Detail Report"
    val status = spec.evalStatus.status
    
    s"""<!DOCTYPE html>
<html lang="en">
	<head>
		${formatHtmlHead(s"${title} - ${featureName}")}
	</head>
	<body>
		${formatReportHeader(title, interpreterName)}
		<ol class="breadcrumb">${(backlinks map { case (backName, backFile) => s"""
			<li>
				<a href="${backFile.getName()}">&lt; ${escape(backName)}</a>
			</li>"""}).mkString}
			<li>
				Evaluation Status
			</li>
			<li>
				<span class="badge badge-${cssStatus(status)}">${status}</span>
			</li>
			<li>
				${escape(featureName)}
			</li>
		</ol>
		<div class="panel panel-default">
			<div class="panel-heading" style="padding-right: 20px; padding-bottom: 0px; border-style: none;">${if (spec.feature.tags.size > 0) s"""
					<span><p>${escape(spec.feature.tags.mkString(" "))}</p></span>""" else ""}
				<span class="label label-black">Feature:</span>
				<span class="pull-right">${durationOrStatus(spec.evalStatus)}</span>
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
		</div>${if (!metas.isEmpty) { 
		val count = metas.size
		val evalStatus = EvalStatus(metaResults.map(_.evalStatus))
		val status = evalStatus.status
		s"""
		<div class="panel panel-${cssStatus(status)} bg-${cssStatus(status)}">
			<ul class="list-group">
				<li class="list-group-item list-group-item-${cssStatus(status)}" style="padding: 10px 10px; margin-right: 10px;">
					<span class="label label-${cssStatus(status)}">Meta:</span>
					${count} meta feature${if (count > 1) "s" else ""} ${if (count > 1) s"""
					<span class="pull-right">${formatDuration(evalStatus.duration)}</span>""" else ""}
				</li>
			</ul>
			<div class="panel-body">
				<ul class="list-group">
					<li class="list-group-item list-group-item-${cssStatus(status)}">
						<table width="100%">
							<tbody class="summary">${(metas map { case (metaFeature, metaResult) => 
			  					val status = metaResult.evalStatus.status
			  					s"""
								<tr>
			  						<td>
										<a class="text-${cssStatus(status)}" href="${metaResult.reportFile.get.getName()}">${escape(metaFeature.feature.name)}</a>
									</td>
									<td>&nbsp; &nbsp; </td>
									<td>
										<span class="pull-right">${formatDuration(metaResult.evalStatus.duration)}</span>${metaFeature.featureFile.map(file => s"""
										<span class="text-${cssStatus(status)}">${file.getPath()}</span>""").getOrElse("")}
									</td>
								</tr>"""}).mkString}
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
					<span class="pull-right">${durationOrStatus(scenario.evalStatus)}</span>""" else ""}
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
							<span class="pull-right">${durationOrStatus(background.evalStatus)}</span>""" else ""}
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
		</div>"""}).mkString}${formatJsFooter}
	</body>
</html>
"""
  }
  
  /**
   * Formats the feature summary report as HTML.
   * 
   * @param results
   * 			the list of evaluated feature results
   * @param interpreterName
   * 			the name of the engine implementation
   */
  override def formatSummary(summary: FeatureSummary, interpreterName: String): String = {
    
    val title = "Feature Summary Report";
    val status = EvalStatus(summary.featureResults.map(_.evalStatus)).status
  
    s"""<!DOCTYPE html>
<html lang="en">
	<head>
		${formatHtmlHead(title)}
	</head>
	<body>
		${formatReportHeader(title, interpreterName)}
		<ol class="breadcrumb">
			<li>
				&nbsp; &nbsp;Summary
			</li>
			<li>
				Evaluation Status
			</li>
			<li>
				<span class="badge badge-${cssStatus(status)}">${status}</span>
			</li>
		</ol>
		<div class="panel panel-default">
			<div class="panel-heading" style="padding-right: 20px; padding-bottom: 0px; border-style: none;">
				<span class="label label-black">Results</span>
				<span class="pull-right">${formatDuration(summary.featureResults.map(_.evalStatus.duration).sum)}</span>
				<div class="panel-body" style="padding-left: 0px; padding-right: 0px; margin-right: -10px;">
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
					<span class="pull-right">${formatDuration(results.map(_.evalStatus.duration).sum)}</span>""" else ""}"""}
				</li>
			</ul>
			<div class="panel-body">
				<ul class="list-group">
					<li class="list-group-item list-group-item-${cssStatus(status)}">
						<table width="100%">
							<tbody class="summary">${(results map { featureResult => s"""
								<tr>
								  <td>
										<a class="text-${cssStatus(status)}" href="${featureResult.reportFile.get.getName()}">${escape(featureResult.featureName)}</a>
								  </td>
								  <td>&nbsp; &nbsp; </td>
								  <td>
										<span class="pull-right">${formatDuration(featureResult.evalStatus.duration)}</span>${featureResult.featureFile.map(file => s"""
										<span class="text-${cssStatus(status)}">${file.getPath()}</span>""").getOrElse("")}
								  </td>
								</tr>"""}).mkString}
							</tbody>
						</table>
					</li>
				</ul>
			</div>
		</div>"""}}).mkString}${formatJsFooter}
	</body>
</html>
    """
  }
  
  private def formatHtmlHead(title: String) = s"""
		<meta charset="utf-8" />
		<meta http-equiv="X-UA-Compatible" content="IE=edge" />
		<meta name="viewport" content="width=device-width, initial-scale=1" />
		<title>${title}</title>
		<!-- Bootstrap -->
		<link href="resources/css/bootstrap.min.css" rel="stylesheet" />
		<!-- Customisations -->
		<link href="resources/css/gwen.css" rel="stylesheet" />"""
    
  private def formatReportHeader(heading: String, interpreterName: String) = s"""
		<table width="100%" cellpadding="5">
		  <tr>
		  <td width="100px">
		  <img src="resources/img/gwen-logo.png" border="0" width="83px" height="115px"></img>
		  </td>
		  <td>
		<div class="panel-heading">
			<h3>${escape(heading)}</h3>
  			<span class="pull-right" style="white-space: nowrap;">
				<center>
					<span class="badge" style="background-color: #1f23ae;">${escape(interpreterName)}</span>
				</center>
			</span>
			<small>${new Date()}</small>
		</div>
		  </td>
		  </tr>
	    </table>"""
						
  private def formatProgressBar(name: String, evalStatuses: List[EvalStatus]): String = formatProgressBar(name, StatusKeyword.countsByStatus(evalStatuses))
							
  private def formatProgressBar(name: String, statusCounts: Map[StatusKeyword.Value, Int]): String = { 
    val total = (statusCounts map { case (_, count) => count }).sum
    s"""
						<tr><td align="right">
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
						</td></tr>"""
  }
						
  private def formatStepLine(step: Step, status: StatusKeyword.Value): String = s"""
							<li class="list-group-item list-group-item-${cssStatus(status)} ${if (status == StatusKeyword.Failed) s"bg-${cssStatus(status)}" else ""}">${if (status == StatusKeyword.Failed) s"""
								<div class="text-${cssStatus(status)} bg-${cssStatus(status)}">""" else ""}
									<span class="pull-right">${durationOrStatus(step.evalStatus)}</span>${if (!step.attachments.isEmpty) s"""
									<div class="dropdown bg-${cssStatus(status)}">
									    <strong>${step.keyword}</strong> ${escape(step.expression)}
										<button class="btn btn-${cssStatus(status)} dropdown-toggle" type="button" id="dropdownMenu1" data-toggle="dropdown">
										    <strong><em>attachments</em></strong>
											<span class="caret"></span>
										</button>
										<ul class="dropdown-menu pull-right" role="menu">${(step.attachments map { case (name, file) => s"""
											<li role="presentation"><a role="menuitem" tabindex="-1" href="attachments/${file.getName()}">${escape(name)}</a></li>"""}).mkString }
										</ul>
									</div>""" else s""" 
									<strong>${step.keyword}</strong> ${escape(step.expression)}"""}${if (status == StatusKeyword.Failed) s"""
								</div>""" else ""}  
							</li>"""

  private def formatJsFooter = """ 
		<!-- Include all compiled plugins (below), or include individual files as needed -->
		<script src="resources/js/jquery-1.11.0.min.js"></script>
		<script src="resources/js/bootstrap.min.js"></script>"""
    
  private def percentageRounded(percentage: Double): String = percentFormatter.format(percentage)
  private def calcPercentage(count: Int, total: Int): Double = 100 * count.toDouble / total.toDouble
  private def durationOrStatus(evalStatus: EvalStatus) = evalStatus.status match {
    case StatusKeyword.Passed | StatusKeyword.Failed => formatDuration(evalStatus.duration)
    case _ => evalStatus.status
  }
  private def formatDuration(duration: Long) = EvalStatus.formatDuration(duration)
  private def escape(text: String) = text.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll("\"", "&quot;").replaceAll("'", "&#39;")
}