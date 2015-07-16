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
      StatusKeyword.Skipped -> "warning", 
      StatusKeyword.Pending -> "info",
      StatusKeyword.Loaded -> "success")
  
  private val percentFormatter = new DecimalFormat("#.##")

  /**
    * Formats the feature detail report as HTML.
    * 
    * @param options gwen command line options
    * @param info the gwen implementation info
    * @param result the feature result to report
    * @param breadcrumbs names and references for linking back to parent reports
    */
  override def formatDetail(options: GwenOptions, info: GwenInfo, result: FeatureResult, breadcrumbs: List[(String, File)]): String = {
    
    val spec = result.spec
    val metaResults = result.metaResults 
    val featureName = spec.featureFile.map(_.getPath()).getOrElse(spec.feature.name)
    val scenarios = spec.scenarios
    val steps = spec.steps
    val isMeta = spec.featureFile.map(_.getName().endsWith(".meta")).getOrElse(false)
    val title = s"${if(isMeta) "Meta" else "Feature"} Detail"
    val status = spec.evalStatus.status
    val summary = result.summary
    val screenshots = result.screenshots
    val rootPath = relativePath(result.report.get, options.reportDir.get).filter(_ == File.separatorChar).flatMap(c => "../")
    
    s"""<!DOCTYPE html>
<html lang="en">
  <head>
    ${formatHtmlHead(s"${title} - ${featureName}", rootPath)}
        ${formatJsHeader(rootPath, screenshots.size > 1)}
  </head>
  <body>
    ${formatReportHeader(info, title, featureName, rootPath)}
    <ol class="breadcrumb">${(breadcrumbs map { case (text, reportFile) => s"""
      <li>
        <span class="caret-left"></span> <a href="${if (text == "Summary") rootPath else "../"}${reportFile.getName()}">${escape(text)}</a>
      </li>"""}).mkString}
      <li>
        <span class="badge badge-${cssStatus(status)}">${status}</span>
      </li>
      <li>
        <small>${escape(result.timestamp.toString)}</small>
      </li>
        ${ if (screenshots.size > 1) { s"""
             <li>
               ${formatSlideShow(screenshots)} 
             </li>"""
           } else ""
         }
    </ol>
    <div class="panel panel-default">
      <div class="panel-heading" style="padding-right: 20px; padding-bottom: 0px; border-style: none;">${if (spec.feature.tags.size > 0) s"""
          <span><p>${escape(spec.feature.tags.mkString(" "))}</p></span>""" else ""}
        <span class="label label-black">Feature</span>
        <span class="pull-right"><small>${durationOrStatus(spec.evalStatus)}</small></span>
        ${escape(spec.feature.name)}${if (!spec.feature.narrative.isEmpty) s"""
        <p>
        <ul class="list-group bg-default">${(spec.feature.narrative  map { line => 
          s"""<li class="list-group-item bg-default">${line}</li>"""}).mkString}
        </ul>
        </p>""" else ""}
        <div class="panel-body" style="padding-left: 0px; padding-right: 0px; margin-right: -10px;">
          <table width="100%" cellpadding="5">
            ${formatProgressBar("Scenario", summary.scenarioCounts)}
            ${formatProgressBar("Step", summary.stepCounts)}
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
          <span class="label label-${cssStatus(status)}">Meta</span>
          ${count} meta feature${if (count > 1) "s" else ""} ${if (count > 1) s"""
          <span class="pull-right"><small>${durationOrStatus(metaStatus)}</small></span>""" else ""}
        </li>
      </ul>
      <div class="panel-body">
        <ul class="list-group">
          <li class="list-group-item list-group-item-${cssStatus(status)}">
            <div class="container-fluid" style="padding: 0px 0px">
              ${(metaResults.zipWithIndex map { case (result, rowIndex) => formatSummaryLine(result, s"meta/${result.report.get.getName()}", None, rowIndex)}).mkString}
            </div>
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
          <span class="label label-${cssStatus(status)}">Scenario</span>${if (scenario.allSteps.size > 1) s"""
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
              <span class="label label-${cssStatus(status)}">Background</span>${if (background.steps.size > 1) s"""
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
    </div>"""}).mkString}
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
      ${formatJsHeader("../", false)}
  </head>
  <body>
    ${formatReportHeader(info, title, if (options.args.isDefined) escape(options.commandString(info)) else "", "")}
    <ol class="breadcrumb">
      <li style="color: gray">
        <span class="caret-left" style="color: #f5f5f5;"></span> Summary
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
        <div class="panel-body" style="padding-left: 0px; padding-right: 0px; margin-right: -10px;">
          <table width="100%" cellpadding="5">
            ${formatProgressBar("Feature", summary.featureCounts)}
            ${formatProgressBar("Scenario", summary.scenarioCounts)}
            ${formatProgressBar("Step", summary.stepCounts)}
          </table>
        </div>
      </div>
    </div>${(StatusKeyword.reportables.reverse map { status => 
    summary.featureResults.zipWithIndex.filter { _._1.evalStatus.status == status } match {
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
          <span class="pull-right"><small>${formatDuration(results.map(_._1.evalStatus.duration).reduceLeft(_+_))}</small></span>""" else ""}"""}
        </li>
      </ul>
      <div class="panel-body">
        <ul class="list-group">
          <li class="list-group-item list-group-item-${cssStatus(status)}">
            <div class="container-fluid" style="padding: 0px 0px">${
                (results.zipWithIndex map { case ((result, resultIndex), rowIndex) => 
                  val report = result.report.get
                  formatSummaryLine(result, s"${relativePath(report, options.reportDir.get).replace(File.separatorChar, '/')}", Some(resultIndex + 1), rowIndex)
                }).mkString}
            </div>
          </li>
        </ul>
      </div>
    </div>"""}}).mkString}
  </body>
</html>
    """
  }
  
  private def formatHtmlHead(title: String, rootPath: String) = s"""
    <meta charset="utf-8" />
    <meta http-equiv="X-UA-Compatible" content="IE=edge" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>${title}</title>
    <link href="${rootPath}resources/css/bootstrap.min.css" rel="stylesheet" />
    <link href="${rootPath}resources/css/gwen.css" rel="stylesheet" />"""
    
  private def formatReportHeader(info: GwenInfo, heading: String, path: String, rootPath: String) = s"""
    <table width="100%" cellpadding="5">
      <tr>
        <td width="100px">
          <a href="${info.gwenHome}"><img src="${rootPath}resources/img/gwen-logo.png" border="0" width="83px" height="115px"></img></a>
        </td>
        <td>
          <h3>${escape(heading)}</h3>
          ${escape(path)}
        </td>
        <td align="right">
          <h3>&nbsp;</h3>
          <a href="${info.implHome}"><span class="badge" style="background-color: #1f23ae;">${escape(info.implName)}</span></a>
          <p>
          <small style="white-space: nowrap; color: #1f23ae; padding-right: 7px;">${info.implVersion}</small>
          </p>
        </td>
      </tr>
    </table>"""

  private def formatProgressBar(name: String, counts: Map[StatusKeyword.Value, Int]): String = { 
    val total = counts.map(_._2).sum
    if (total > 0) {s"""
            <tr>
              <td align="right">
                <span style="white-space: nowrap;">${total} ${name}${if (total > 1) "s" else ""}</span>
              </td>
              <td width="99%">
                <div class="progress">${(StatusKeyword.reportables map { status =>
                val count = counts.get(status).getOrElse(0)
                val percentage = calcPercentage(count, total)
                s"""
                <div class="progress-bar progress-bar-${cssStatus(status)}" style="width: ${percentage}%">
                  <span>${count} ${status} - ${percentageRounded(percentage)}%</span>
                </div>"""}).mkString}
              </div>
              </td>
            </tr>"""} else ""
  }
  
  private def formatSummaryLine(result: FeatureResult, reportPath: String, sequenceNo: Option[Int], rowIndex: Int): String = s"""
                <div class="row${if (rowIndex % 2 == 1) s" bg-altrow-${cssStatus(result.evalStatus.status)}" else "" }">
                  <div class="col-md-3" style="padding-left: 0px">${sequenceNo.map(seq => s"""
                    <div class="line-no"><small>${seq}</small></div>""").getOrElse("")}
                    <span style="padding-left: 15px; white-space: nowrap;"><small>${escape(result.timestamp.toString)}</small></span>
                  </div>
                  <div class="col-md-4">
                    <a class="text-${cssStatus(result.evalStatus.status)}" href="${reportPath}">${escape(result.spec.feature.name)}</a>
                  </div>
                  <div class="col-md-5">
                    <span class="pull-right"><small>${durationOrStatus(result.evalStatus)}</small></span> ${result.spec.featureFile.map(_.getPath()).getOrElse("")}
                  </div>
                </div>"""

  private def formatStepLine(step: Step, status: StatusKeyword.Value): String = s"""
              <li class="list-group-item list-group-item-${cssStatus(status)} ${if (status == StatusKeyword.Failed) s"bg-${cssStatus(status)}" else ""}">
                <div class="bg-${cssStatus(status)}">
                  <span class="pull-right"><small>${durationOrStatus(step.evalStatus)}</small></span>
                  <div class="line-no"><small>${step.pos.line}</small></div>
                  <div class="keyword-right"><strong>${step.keyword}</strong></div> ${escape(step.expression)}
                  ${formatAttachments(step.attachments, status)}
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
                  
  private def formatAttachments(attachments: List[(String, File)], status: StatusKeyword.Value) = s"""
                  ${if (!attachments.isEmpty) s"""
                  <div class="dropdown bg-${cssStatus(status)}">
                    <button class="btn btn-${cssStatus(status)} dropdown-toggle" type="button" id="dropdownMenu1" data-toggle="dropdown">
                      <strong>attachment${if (attachments.size > 1) "s" else ""}</strong>
                      <span class="caret"></span>
                    </button>
                    <ul class="dropdown-menu pull-right" role="menu">${(attachments map { case (name, file) => s"""
                      <li role="presentation" class="text-${cssStatus(status)}"><a role="menuitem" tabindex="-1" href="attachments/${file.getName()}" target="_blank">${escape(name)}</a></li>"""}).mkString }
                    </ul>
                  </div>""" else ""}"""

    private def formatJsHeader(rootPath: String, withReel: Boolean) = s""" 
    <script src="${rootPath}resources/js/jquery.min.js"></script>
    <script src="${rootPath}resources/js/bootstrap.min.js"></script>
    ${ if (withReel) { s"""<script src="${rootPath}resources/js/jquery.reel-min.js"></script>""" } else ""}"""
      
  private def percentageRounded(percentage: Double): String = percentFormatter.format(percentage)
  private def calcPercentage(count: Int, total: Int): Double = 100 * count.toDouble / total.toDouble
  private def durationOrStatus(evalStatus: EvalStatus) = evalStatus.status match {
    case StatusKeyword.Passed | StatusKeyword.Failed => formatDuration(evalStatus.duration)
    case _ => evalStatus.status
  }
  private def formatDuration(duration: Duration) = DurationFormatter.format(duration)
  private def escape(text: String) = String.valueOf(text).replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll("\"", "&quot;").replaceAll("'", "&#39;")
  private def relativePath(reportFile: File, reportDir: File) = reportFile.getPath.substring(reportDir.getPath().length() + 1)
  private def formatSlideShow(screenshots: List[File]) = s"""
  <div class="modal fade" id="slideshow" tabindex="-1" role="dialog" aria-labelledby="slideshowLabel" aria-hidden="true">
  <div class="modal-dialog" style="width: 60%;">
  <div class="modal-content">
    <div class="modal-body">
    <a href="#" title="Close"><span id="close-btn" class="pull-right glyphicon glyphicon-remove-circle" aria-hidden="true"></span></a>
    <p>
    <center>
      <div id="loading-div"><span class="glyphicon glyphicon-download" aria-hidden="true"></span> Loading slides, please wait..</div>
      <div id="controls-div" style="display: none;">
        <button id="fast-back-btn" class="btn btn-default btn-lg" title="Rewind to start"><span class="glyphicon glyphicon-fast-backward" aria-hidden="true"></span></button>
        <button id="step-back-btn" class="btn btn-default btn-lg" title="Step backward"><span class="glyphicon glyphicon-step-backward" aria-hidden="true"></span></button>
        <button id="play-pause-btn" class="btn btn-default btn-lg" title="Play"><span id="play-pause" class="glyphicon glyphicon-play" aria-hidden="true"></span></button>
        <button id="step-fwd-btn" class="btn btn-default btn-lg" title="Step forward"><span class="glyphicon glyphicon-step-forward" aria-hidden="true"></span></button>
        <button id="fast-fwd-btn" class="btn btn-default btn-lg" title="Forward to end"><span class="glyphicon glyphicon-fast-forward" aria-hidden="true"></span></button>
        <select id="current-frame" title="Jump to..">${(for(i <- 1 to screenshots.length) yield s"""
          <option>${i}</option>""").mkString}        
        </select> of ${screenshots.length}
      </div>
    </center>
    <hr>
    </p>
    <img id="slides" src="${screenshots.headOption.map(_.getName).mkString("attachments/","","")}" width="100%" height="100%" />
    <script>
      var revolution = $$('#slides').width();
      $$('#slides').reel({
        images: [ ${screenshots.map(_.getName()).mkString("'attachments/","','attachments/","'")} ],
        frames: ${screenshots.length},
        speed: 0,
        indicator: 5,
        responsive: true,
        loops: true,
        cursor: 'auto',
        revolution: revolution,
        steppable: false,
        preload: 'linear'
      }).bind("frameChange", function(e, d, frame){
        if (frame == ${screenshots.length}) { stop(); } 
        $$('#current-frame').val(frame);
      }).bind("loaded", function(ev){
        $$('#loading-div').hide();
        $$('#controls-div').show();
        if($$('#controls-div').is(':visible')) play();
      });
      function play() {
        $$('#play-pause').removeClass("glyphicon-play");
        $$('#play-pause').addClass("glyphicon-pause");
        $$('#play-pause').attr("title", "Pause");
        if ($$('#slides').reel('frame') == ${screenshots.length}) { $$('#slides').reel('frame', 1); }
        $$('#slides').trigger("play", 3 / ${screenshots.length});
      }
      function stop() {
        $$('#slides').trigger("stop");
        $$('#play-pause').removeClass("glyphicon-pause");
        $$('#play-pause').addClass("glyphicon-play");
        $$('#play-pause').attr("title", "Play");
      }
      $$(function() {
        $$('#fast-back-btn').click(function(e) { $$('#slides').reel('frame', 1); stop(); });
        $$('#step-back-btn').click(function(e) { $$('#slides').trigger('stepRight'); stop(); });
        $$('#play-pause-btn').click(function() { 
          if ($$('#play-pause').hasClass("glyphicon-play")) { play(); } 
          else if ($$('#play-pause').hasClass("glyphicon-pause")) { stop(); } 
        });
        $$('#step-fwd-btn').click(function(e) { $$('#slides').trigger('stepLeft'); stop(); });
        $$('#fast-fwd-btn').click(function(e) { $$('#slides').reel('frame', ${screenshots.length}); stop(); });
        $$('#current-frame').change(function(e) { $$('#slides').reel('frame', parseInt($$(this).val())); stop(); });
        $$('#close-btn').click(function(e) { e.preventDefault(); $$('#slideshow').modal('hide'); });
        $$('#slideshow').on('show.bs.modal', function (e) { $$('#slides').reel('frame', 1); stop(); });
        $$('#slideshow').on('shown.bs.modal', function (e) { $$('#slides').reel('frame', 1); if($$('#controls-div').is(':visible')) play(); });
        $$('#slideshow').on('hide.bs.modal', function (e) { $$('#slides').trigger('stop') });
        $$('#slideshow').on('hidden.bs.modal', function (e) { $$('#slides').trigger('stop') });
      });
    </script>
   </div>
  </div>
</div>
</div>
<button type="button" class="btn btn-default btn-lg" data-toggle="modal" data-target="#slideshow">
  Slideshow
</button>
  """
}