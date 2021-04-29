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

import gwen.GwenInfo
import gwen.GwenSettings
import gwen.GwenOptions
import gwen.model._
import gwen.report.ReportFormatter

import java.io.File

/** Formats the slideshow. */
trait HtmlSlideshowFormatter extends ReportFormatter {
  
  /**
    * Formats the feature detail report as HTML.
    * 
    * @param options gwen command line options
    * @param info the gwen implementation info
    * @param unit the feature input
    * @param result the feature result to report
    * @param breadcrumbs names and references for linking back to parent reports
    * @param reportFiles the target report files (head = detail, tail = metas)
    */
  override def formatDetail(options: GwenOptions, info: GwenInfo, unit: FeatureUnit, result: FeatureResult, breadcrumbs: List[(String, File)], reportFiles: List[File]): Option[String] = {
    val screenshots = result.screenshots
    if (screenshots.isEmpty || result.isMeta) None
    else {
    
      val reportDir = HtmlSlideshowConfig.reportDir(options).get
      val featureName = result.spec.featureFile.map(_.getPath()).getOrElse(result.spec.feature.name)
      val rootPath = relativePath(reportFiles.head, reportDir).filter(_ == File.separatorChar).flatMap(_ => "../")
      val summaryCrumb = ("Summary", new File(s"$rootPath/html", "feature-summary.html"))
      val dir = HtmlReportConfig.createReportDir(options, result.spec, unit.dataRecord).get
      val file = HtmlReportConfig.createReportFile(dir, "", result.spec, unit.dataRecord).get
      val featureCrumb = (SpecType.Feature.toString, file)
      Some(s"""<!DOCTYPE html>
<html lang="en">
  <head>
    ${formatHtmlHead(s"Slideshow - $featureName", rootPath)}
    ${formatJsHeader(rootPath)}
  </head>
  <body>
    ${HtmlReportFormatter.formatReportHeader(info, "Feature Slideshow", featureName, rootPath)}
    ${HtmlReportFormatter.formatStatusHeader(unit, result, rootPath, List(summaryCrumb, featureCrumb), Nil)}
    ${HtmlSlideshowFormatter.formatSlideshow(screenshots, rootPath)}
  </body>
</html>""")
    }
  }
  
  /**
    * Not used by this implementation.
    * 
    * @param options gwen command line options
    * @param info the gwen implementation info
    * @param summary the accumulated feature results summary
    */
  override def formatSummary(options: GwenOptions, info: GwenInfo, summary: FeatureSummary): Option[String] = None
  
  private def formatJsHeader(rootPath: String) = s""" 
    <script src="${rootPath}resources/js/jquery.min.js"></script>
    <script src="${rootPath}resources/js/bootstrap.min.js"></script>"""
    
  private def formatHtmlHead(title: String, rootPath: String) = s"""
    <meta charset="utf-8" />
    <meta http-equiv="X-UA-Compatible" content="IE=edge" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>$title</title>
    <link href="${rootPath}resources/css/bootstrap.min.css" rel="stylesheet" />
    <link href="${rootPath}resources/css/gwen.css" rel="stylesheet" />"""
  
}

object HtmlSlideshowFormatter {

  private def maxFramesPerSec(screenshots: List[File]) = if (screenshots.length < 18) screenshots.length else 18
  private def defaultFramesPerSec(screenshots: List[File]) = {
    val fps = GwenSettings.`gwen.report.slideshow.framespersecond`
    val max = maxFramesPerSec(screenshots)
    if (fps < max) {
      fps
    } else {
      max
    }
  }
  
  private[report] def formatSlideshow(screenshots: List[File], rootPath: String) = s"""
<center>
  <div class="slideshow-body">
  <p>
    <div id="loading-div"><span class="glyphicon glyphicon-download" aria-hidden="true"></span> Loading slides, please wait..</div>
    <div id="controls-div" style="display: none;">
      <button id="fast-back-btn" class="btn btn-default btn-lg" title="Rewind to start"><span class="glyphicon glyphicon-fast-backward" aria-hidden="true"></span></button>
      <button id="step-back-btn" class="btn btn-default btn-lg" title="Step backward"><span class="glyphicon glyphicon-step-backward" aria-hidden="true"></span></button>
      <button id="play-pause-btn" class="btn btn-default btn-lg" title="Play"><span id="play-pause" class="glyphicon glyphicon-play" aria-hidden="true"></span></button>
      <button id="step-fwd-btn" class="btn btn-default btn-lg" title="Step forward"><span class="glyphicon glyphicon-step-forward" aria-hidden="true"></span></button>
      <button id="fast-fwd-btn" class="btn btn-default btn-lg" title="Forward to end"><span class="glyphicon glyphicon-fast-forward" aria-hidden="true"></span></button>
      <select id="current-frame" title="Jump to..">${(for(i <- 1 to screenshots.length) yield s"""
        <option>$i</option>""").mkString}
      </select> of ${screenshots.length}
      <span style="margin-left: 30px;"> </span>
      <button id="decrease-speed-btn" class="btn btn-default btn-lg" title="Decrease Speed"><span class="glyphicon glyphicon-minus" aria-hidden="true"></span></button>
      <button id="increase-speed-btn" class="btn btn-default btn-lg" title="Increase Speed"><span class="glyphicon glyphicon-plus" aria-hidden="true"></span></button>
      <select id="frames-per-sec" title="Frames per second..">${(for(i <- 1 to maxFramesPerSec(screenshots) ) yield s"""
        <option>$i</option>""").mkString}
      </select> frames/sec
    </div>
  </p>
  <hr>
  <img id="slides" src="${screenshots.headOption.map(_.getName).mkString("attachments/","","")}" width="100%" height="100%"/>
  <script src="${rootPath}resources/js/jquery.reel-min.js"></script>
  <script>
    var revolution = $$('#slides').width();
    var unitSpeed = 1 / ${screenshots.length};
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
    });
    function play() {
      $$('#play-pause').removeClass("glyphicon-play");
      $$('#play-pause').addClass("glyphicon-pause");
      $$('#play-pause').attr("title", "Pause");
      if ($$('#slides').reel('frame') == 1) { $$('#slides').reel('frame', ${screenshots.length}); }
      if ($$('#slides').reel('frame') == ${screenshots.length}) { $$('#slides').reel('frame', 1); }
      $$('#slides').trigger("play", getFramesPerSec() * unitSpeed);
    }
    function getFramesPerSec() {
      return parseInt($$('#frames-per-sec').val());
    }
    function decreaseSpeed() {
      var framesPerSec = getFramesPerSec();
      if (framesPerSec > 1) {
        framesPerSec = framesPerSec - 1;
        $$('#frames-per-sec').val(framesPerSec).trigger('change');
        toggleSpeedButtons(framesPerSec);
      }
    }
    function increaseSpeed() {
      var framesPerSec = getFramesPerSec();
      if (framesPerSec < ${maxFramesPerSec(screenshots)}) {
        framesPerSec = framesPerSec + 1;
        $$('#frames-per-sec').val(framesPerSec).trigger('change');
        toggleSpeedButtons(framesPerSec);
      }
    }
    function stop() {
      $$('#slides').trigger("stop");
      $$('#play-pause').removeClass("glyphicon-pause");
      $$('#play-pause').addClass("glyphicon-play");
      $$('#play-pause').attr("title", "Play");
    }
    function toggleSpeedButtons(framesPerSec) {
      $$('#increase-speed-btn').prop('disabled', framesPerSec == ${maxFramesPerSec(screenshots)});
      $$('#decrease-speed-btn').prop('disabled', framesPerSec == 1);
    }
    $$(function() {
      $$('#frames-per-sec').val('${defaultFramesPerSec(screenshots)}');
      $$('#increase-speed-btn').click(function(e) { increaseSpeed() });
      $$('#decrease-speed-btn').click(function(e) { decreaseSpeed() });
      toggleSpeedButtons(getFramesPerSec());
      $$('#fast-back-btn').click(function(e) { $$('#slides').reel('frame', 1); stop(); });
      $$('#step-back-btn').click(function(e) { $$('#slides').trigger('stepRight'); stop(); });
      $$('#play-pause-btn').click(function() { 
        if ($$('#play-pause').hasClass("glyphicon-play")) { play(); } 
        else if ($$('#play-pause').hasClass("glyphicon-pause")) { stop(); } 
      });
      $$('#step-fwd-btn').click(function(e) { $$('#slides').trigger('stepLeft'); stop(); });
      $$('#fast-fwd-btn').click(function(e) { $$('#slides').reel('frame', ${screenshots.length}); stop(); });
      $$('#current-frame').change(function(e) { $$('#slides').reel('frame', parseInt($$(this).val())); stop(); });
      $$('#frames-per-sec').change(function(e) { $$('#slides').reel('speed', parseInt($$(this).val()) * unitSpeed); toggleSpeedButtons(getFramesPerSec()); });
    });
  </script>
</div>
</center>
"""
}