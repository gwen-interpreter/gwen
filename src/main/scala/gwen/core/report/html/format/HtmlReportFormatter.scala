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

import HtmlReportFormatter._

import gwen.core._
import gwen.core.Formatting._
import gwen.core.node._
import gwen.core.report.ReportFormatter
import gwen.core.status._
import gwen.core.result.SpecResult

import scala.concurrent.duration.Duration
import scala.util.Try
import scalatags.Text.all._
import scalatags.Text.TypedTag

import java.text.DecimalFormat
import java.util.Date

/** Formats the feature summary and detail reports in HTML. */
trait HtmlReportFormatter extends ReportFormatter with SummaryFormatter with DetaiFormatter {

  private val percentFormatter = new DecimalFormat("#.##")
  
  private [format] def formatProgressBar(nodeType: NodeType, counts: Map[StatusKeyword, Int]): Option[TypedTag[String]] = { 
    for {
      total <- Some(counts.values.sum).filter(_ > 0)
    } yield {
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
            } yield {
              div(`class` := s"progress-bar progress-bar-${cssStatus(status)}", style := s"width: $percentage%;",
                span(
                  s"$count $status - ${percentageRounded(percentage)}%"
                )
              )
            }
          )
        )
      )
    }
  }
  
  private [format] def formatSummaryLine(result: SpecResult, reportPath: Option[String], sequenceNo: Option[Int], rowIndex: Int): TypedTag[String] = {
    val featureName = Option(result.spec.feature.name).map(_.trim).filter(!_.isEmpty).getOrElse(result.spec.specFile.map(_.getName()).map(n => Try(n.substring(0, n.lastIndexOf('.'))).getOrElse(n)).getOrElse("-- details --"))
    val reportingStatus = result.evalStatus match {
      case OK(nanos) if result.sustainedCount > 0 => Sustained(nanos, null)
      case status => status
    }
    div(`class` := s"row${if (rowIndex % 2 == 1) s" bg-altrow-${cssStatus(result.evalStatus.keyword)}" else "" }",
      div(`class` := "col-md-3", style := "padding-left: 0px",
        for {
          seq <- sequenceNo
        } yield {
          div(`class` := "line-no",
            small(
              seq.toString
            )
          )
        },
        span(style := "padding-left: 15px; white-space: nowrap;",
          small(
            result.finished.toString
          )
        )
      ),
      div(`class` := "col-md-4",
        reportPath match {
          case Some(rpath) =>
            a(`class` := s"text-${cssStatus(reportingStatus.keyword)}", style := s"color: ${linkColor(reportingStatus.keyword)};", href := rpath,
              span(`class` := s"text-${cssStatus(reportingStatus.keyword)}",
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

  private def calcPercentage(count: Int, total: Int): Double = {
    100 * count.toDouble / total.toDouble
  }

  private def percentageRounded(percentage: Double): String = {
    percentFormatter.format(percentage)
  }

}

object HtmlReportFormatter {

  private [format] val cssStatus = Map(
    StatusKeyword.OK -> "success", 
    StatusKeyword.Failed -> "danger",
    StatusKeyword.Sustained -> "danger",
    StatusKeyword.Skipped -> "warning",
    StatusKeyword.Pending -> "info",
    StatusKeyword.Loaded -> "success",
    StatusKeyword.Disabled -> "default")

  private [format] val linkColor = Map(
    StatusKeyword.OK -> "#3c763d",
    StatusKeyword.Failed -> "#a94442",
    StatusKeyword.Sustained -> "#a94442",
    StatusKeyword.Skipped -> "#8a6d3b",
    StatusKeyword.Pending -> "#31708f",
    StatusKeyword.Loaded -> "#3c763d",
    StatusKeyword.Disabled -> "grey"
  )

  private [format] def formatHtmlHead(pageTitle: String, rootPath: String): TypedTag[String] = {
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
  
  private [format] def formatReportHeader(heading: String, path: String, rootPath: String, info: GwenInfo): TypedTag[String] = {
    val version = s"v${info.implVersion}"
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
            small(style := "white-space: nowrap; color: #1f23ae;",
              info.releaseNotesUrl map { url => a(href := url, version) } getOrElse version
            )
          )
        )
      )
    )
  }
         
  private [format] def formatBadgeStatus(status: StatusKeyword, renderErrorLink: Boolean, sustainedCount: Int): TypedTag[String] = {
    val sustainedError = s"${sustainedCount} sustained error${if (sustainedCount > 1) "s" else ""}"
    li(
      span(`class` := s"badge badge-${cssStatus(status)}",
        if (renderErrorLink && status == StatusKeyword.Failed) {
          Seq(
            a(id := "failed-link", href := "#", style := "color:white;",
              status.toString
            ),
            formatFailedLinkScript("failed")
          )
        } else {
          status.toString
        }
      ),
      for {
        opt <- Option(sustainedCount > 0)
        if opt
      } yield {
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
                formatFailedLinkScript("sustained")
              )
            } else {
              sustainedError
            }
          )
        )
      }
    )
  }

  private [format] def formatDateStatus(label: String, date: Date): TypedTag[String] = {
    li(
      small(
        span(`class` := "grayed", s"$label: "),
        date.toString
      )
    )
  }

  private [format] def formatElapsedStatus(elapsedTime: Duration): TypedTag[String] = {
    span(`class` := "pull-right", style := "padding-right: 20px; padding-top: 8px;",
      small(formatDuration(elapsedTime))
    )
  }

  private def formatFailedLinkScript(statusType: String): TypedTag[String] = {
    script(
      raw(
        s"""|  
            |  $$(document).ready(function() {
            |    $$('#${statusType}-link').click(
            |      function(e) {
            |        e.preventDefault();
            |        $$('html, body').animate({scrollTop:$$('.badge-${statusType}-issue').closest('.panel').offset().top}, 500);
            |      }
            |    );
            |  });
            |""".stripMargin
      )
    )
  }
          
}
