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
package gwen.core.report.html.format

import gwen.core.Formatting
import gwen.core.GwenInfo
import gwen.core.GwenSettings
import gwen.core.GwenOptions
import gwen.core.node.FeatureUnit
import gwen.core.node.gherkin.Spec
import gwen.core.node.gherkin.SpecType
import gwen.core.report.ReportFormatter
import gwen.core.report.html.HtmlSlideshowConfig
import gwen.core.report.html.HtmlReportConfig
import gwen.core.result.ResultsSummary
import gwen.core.result.SpecResult

import scalatags.Text.all._
import scalatags.Text.TypedTag

import java.io.File

/** Formats the slideshow. */
trait SlideshowFormatter extends ReportFormatter {
  
  /**
    * Formats the feature detail report as HTML.
    * 
    * @param options gwen command line options
    * @param info gwen info
    * @param unit the feature input
    * @param result the feature result to report
    * @param breadcrumbs names and references for linking back to parent reports
    * @param reportFiles the target report files (head = detail, tail = metas)
    */
  override def formatDetail(options: GwenOptions, info: GwenInfo, unit: FeatureUnit, result: SpecResult, breadcrumbs: List[(String, File)], reportFiles: List[File]): Option[String] = {
    val screenshots = result.screenshots
    if (!GwenSettings.`gwen.report.slideshow.create` || screenshots.isEmpty || result.isMeta) None
    else {
      val reportDir = HtmlSlideshowConfig.reportDir(options).get
      val featureName = result.spec.specFile.map(_.getPath()).getOrElse(result.spec.feature.name)
      val rootPath = relativePath(reportFiles.head, reportDir).filter(_ == File.separatorChar).flatMap(_ => "../")
      val summaryCrumb = ("Summary", new File(s"$rootPath/html", "index.html"))
      val dir = HtmlReportConfig.createReportDir(options, result.spec, unit.dataRecord).get
      val file = HtmlReportConfig.createReportFile(dir, "", result.spec, unit.dataRecord).get
      val featureCrumb = (SpecType.Feature.toString, file)
      val htmlPage = 
        html(lang := "en",
          HtmlReportFormatter.formatHtmlHead(s"Slideshow - $featureName", rootPath),
          body(
            HtmlReportFormatter.formatReportHeader("Feature Slideshow", featureName, rootPath, info),
            DetailFormatter.formatDetailStatusBar(unit, result, rootPath, List(summaryCrumb, featureCrumb), Nil, false),
            SlideshowFormatter.formatSlideshow(screenshots, rootPath)
          )
        )
      Some(Formatting.prettyPrintHTML("<!DOCTYPE html>" + htmlPage.render))
    }
  }
  
  /**
    * Not used by this implementation.
    * 
    * @param options gwen command line options
    * @param info gwen info
    * @param summary the accumulated feature results summary
    */
  override def formatSummary(options: GwenOptions, info: GwenInfo, summary: ResultsSummary): Option[String] = None

}

object SlideshowFormatter {

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

  private [format] def formatSlideshowModal(screenshots: List[File], spec: Spec, unit: FeatureUnit, rootPath: String): Seq[TypedTag[String]] = {
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
              formatSlideshow(screenshots, rootPath)
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
             |  $('#close-btn').click(function(e) { e.preventDefault(); $('#slideshow').modal('hide'); });
             |  $('#full-screen').click(function(e) { $('#close-btn').click(); });
             |  $('#slideshow').on('show.bs.modal', function (e) { $('#slides').reel('frame', 1); stop(); });
             |  $('#slideshow').on('hide.bs.modal', function (e) { $('#slides').trigger('stop') });
             |  $('#slideshow').on('hidden.bs.modal', function (e) { $('#slides').trigger('stop') });
             |""".stripMargin
        )
      )
    )
  }
  
  private def formatSlideshow(screenshots: List[File], rootPath: String): TypedTag[String] = {
    tag("center")(
      div(`class` := "slideshow-body",
        p(
          div(id := "loading-div",
            span(`class` := "glyphicon glyphicon-download", attr("aria-hidden") := "true"),
              " Loading slides, please wait.."
          ),
          div(id := "controls-div", style := "display: none;",
            button(id := "fast-back-btn", `class` := "btn btn-default btn-lg", title := "Rewind to start",
              span(`class` := "glyphicon glyphicon-fast-backward", attr("aria-hidden") := "true")
            ),
            button(id := "step-back-btn", `class` := "btn btn-default btn-lg", title := "Step backward",
              span(`class` := "glyphicon glyphicon-step-backward", attr("aria-hidden") := "true")
            ),
            button(id := "play-pause-btn", `class` := "btn btn-default btn-lg", title := "Play",
              span(id := "play-pause", `class` := "glyphicon glyphicon-play", attr("aria-hidden") := "true")
            ),
            button(id := "step-fwd-btn", `class` := "btn btn-default btn-lg", title := "Step forward",
              span(`class` := "glyphicon glyphicon-step-forward", attr("aria-hidden") := "true")
            ),
            button(id := "fast-fwd-btn", `class` := "btn btn-default btn-lg", title := "Forward to end",
              span(`class` := "glyphicon glyphicon-fast-forward", attr("aria-hidden") := "true")
            ),
            select(id := "current-frame", title := "Jump to..",
              for {
                i <- 1 to screenshots.length
              } yield {
                option(i)
              }
            ),
            s" of ${screenshots.length}",
            span(style := "margin-left: 30px;",
              " "
            ),
            button(id := "decrease-speed-btn", `class` := "btn btn-default btn-lg", title := "Decrease Speed",
              span(`class` := "glyphicon glyphicon-minus", attr("aria-hidden") :="true")
            ),
            button(id := "increase-speed-btn", `class` := "btn btn-default btn-lg", title := "Increase Speed",
              span(`class` := "glyphicon glyphicon-plus", attr("aria-hidden") :="true")
            ),
            select(id := "frames-per-sec", title := "Frames per second..",
              for {
                i <- 1 to maxFramesPerSec(screenshots)
              } yield {
                option(i)
              }
            ),
            " frames/sec"
          )
        ),
        hr,
        img(id := "slides", src := s"${screenshots.headOption.map(_.getName).mkString("attachments/","","")}", width := "100%", height := "100%"),
        script(src := s"${rootPath}resources/js/jquery.reel-min.js"),
        script(
          raw(
            s"""|
                |  var revolution = $$('#slides').width();
                |  var unitSpeed = 1 / ${screenshots.length};
                |  $$('#slides').reel({
                |    images: [ ${screenshots.map(_.getName()).mkString("'attachments/","','attachments/","'")} ],
                |    frames: ${screenshots.length},
                |    speed: 0,
                |    indicator: 5,
                |    responsive: true,
                |    loops: true,
                |    cursor: 'auto',
                |    revolution: revolution,
                |    steppable: false,
                |    preload: 'linear'
                |  }).bind("frameChange", function(e, d, frame){
                |    if (frame == ${screenshots.length}) { stop(); } 
                |    $$('#current-frame').val(frame);
                |  }).bind("loaded", function(ev){
                |    $$('#loading-div').hide();
                |    $$('#controls-div').show();
                |  });
                |  function play() {
                |    $$('#play-pause').removeClass("glyphicon-play");
                |    $$('#play-pause').addClass("glyphicon-pause");
                |    $$('#play-pause').attr("title", "Pause");
                |    if ($$('#slides').reel('frame') == 1) { $$('#slides').reel('frame', ${screenshots.length}); }
                |    if ($$('#slides').reel('frame') == ${screenshots.length}) { $$('#slides').reel('frame', 1); }
                |    $$('#slides').trigger("play", getFramesPerSec() * unitSpeed);
                |  }
                |  function getFramesPerSec() {
                |    return parseInt($$('#frames-per-sec').val());
                |  }
                |  function decreaseSpeed() {
                |    var framesPerSec = getFramesPerSec();
                |    if (framesPerSec > 1) {
                |      framesPerSec = framesPerSec - 1;
                |      $$('#frames-per-sec').val(framesPerSec).trigger('change');
                |      toggleSpeedButtons(framesPerSec);
                |    }
                |  }
                |  function increaseSpeed() {
                |    var framesPerSec = getFramesPerSec();
                |    if (framesPerSec < ${maxFramesPerSec(screenshots)}) {
                |      framesPerSec = framesPerSec + 1;
                |      $$('#frames-per-sec').val(framesPerSec).trigger('change');
                |      toggleSpeedButtons(framesPerSec);
                |    }
                |  }
                |  function stop() {
                |    $$('#slides').trigger("stop");
                |    $$('#play-pause').removeClass("glyphicon-pause");
                |    $$('#play-pause').addClass("glyphicon-play");
                |    $$('#play-pause').attr("title", "Play");
                |  }
                |  function toggleSpeedButtons(framesPerSec) {
                |    $$('#increase-speed-btn').prop('disabled', framesPerSec == ${maxFramesPerSec(screenshots)});
                |    $$('#decrease-speed-btn').prop('disabled', framesPerSec == 1);
                |  }
                |  $$(function() {
                |    $$('#frames-per-sec').val('${defaultFramesPerSec(screenshots)}');
                |    $$('#increase-speed-btn').click(function(e) { increaseSpeed() });
                |    $$('#decrease-speed-btn').click(function(e) { decreaseSpeed() });
                |    toggleSpeedButtons(getFramesPerSec());
                |    $$('#fast-back-btn').click(function(e) { $$('#slides').reel('frame', 1); stop(); });
                |    $$('#step-back-btn').click(function(e) { $$('#slides').trigger('stepRight'); stop(); });
                |    $$('#play-pause-btn').click(function() { 
                |      if ($$('#play-pause').hasClass("glyphicon-play")) { play(); } 
                |      else if ($$('#play-pause').hasClass("glyphicon-pause")) { stop(); } 
                |    });
                |    $$('#step-fwd-btn').click(function(e) { $$('#slides').trigger('stepLeft'); stop(); });
                |    $$('#fast-fwd-btn').click(function(e) { $$('#slides').reel('frame', ${screenshots.length}); stop(); });
                |    $$('#current-frame').change(function(e) { $$('#slides').reel('frame', parseInt($$(this).val())); stop(); });
                |    $$('#frames-per-sec').change(function(e) { $$('#slides').reel('speed', parseInt($$(this).val()) * unitSpeed); toggleSpeedButtons(getFramesPerSec()); });
                |  });
                |""".stripMargin
          )
        )
      )
    )
  }

}
