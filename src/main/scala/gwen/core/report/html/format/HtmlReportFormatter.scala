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
import gwen.core.node.gherkin.Step
import gwen.core.node.gherkin.SpecType

import gwen.core.report.ReportFormat
import gwen.core.report.ReportFormatter
import gwen.core.report.html.HtmlReportConfig
import gwen.core.result.ResultsSummary
import gwen.core.status._
import gwen.core.result.SpecResult

import scala.concurrent.duration.Duration
import scalatags.Text.all._
import scalatags.Text.TypedTag

import java.text.DecimalFormat
import java.util.Date
import java.io.File
import scala.io.Source

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
  
  private [format] def formatSummaryLine(options: GwenOptions, result: SpecResult, reportPath: Option[String], sequenceNo: Option[Int], rowIndex: Int, maxNameLength: Int): TypedTag[String] = {
    val videos = result.videos
    val featureName = result.displayName
    val inError = result.evalStatus.isError
    val featureColPercentage = {
      if (inError) {
        Some(Option(Option((maxNameLength / 2) + 2).filter(_ < 25).getOrElse(24)).filter(_ > 7).getOrElse(6) + (if (videos.isEmpty) 1 else 0))
      } else {
        None
      }
    }
    val reportingStatus = result.evalStatus match {
      case Passed(nanos, _) if result.sustainedCount > 0 => Sustained(nanos, null)
      case status => status
    }
    val reportBase = result.reports.flatMap(_.get(ReportFormat.html).flatMap(_.headOption)) map { reportFile => 
      val reportDir = HtmlReportConfig.reportDir(options).get
      Some(relativePath(reportFile.getParentFile, reportDir).replace(File.separatorChar, '/'))
    } getOrElse None
    tr(`class` := s"summary-line-2 ${if (rowIndex % 2 == 1) s"bg-altrow-${cssStatus(result.evalStatus.keyword)}" else "" }", style := "border-top: hidden;",
      td(`class` := "summary-line-2", style := "padding-left: 0px; white-space: nowrap",
        table(`class` := "table-responsive",
          tbody(
            tr(`class` := "summary-line-0", style := "border-top: hidden;",
              td(`class` := "summary-line-0", style := "vertical-align:top; padding-right: 15px;",
                for {
                  seq <- sequenceNo
                } yield {
                  div(`class` := "line-no",
                    small(
                      seq.toString
                    )
                  )
                },
              ),
              td(`class` := "summary-line-0", style := "vertical-align:top;",
                span(
                  small(
                    result.finished.toString
                  )
                )
              ),
            ),
          ),
        )
      ),
      td(`class` := "summary-line-2", width := s"${if (videos.nonEmpty) "60px" else "0px"}",
        if (videos.nonEmpty) {
          formatVideoAttachments(reportBase, videos, Some(result.evalStatus.keyword))
        } else ""
      ),
      td(`class` := "summary-line-2", featureColPercentage.map(percentage => (attr("width") := s"$percentage%")),
        reportPath match {
          case Some(rpath) =>
            a(`class` := s"inverted-${cssStatus(reportingStatus.keyword)}", style := s"color: ${linkColor(reportingStatus.keyword)};", href := rpath,
              span(`class` := s"text-${cssStatus(reportingStatus.keyword)}",
                raw(escapeHtml(featureName))
              )
            )
          case None =>
            raw(escapeHtml(featureName))
        }
      ),
      if (inError) {
        val errorAttachments = {
          if (inError) Step.errorTrails(result.spec).flatMap(_.lastOption.map(_.attachments)).headOption.getOrElse(Nil)
          else Nil
        }
        td(`class` := "summary-line-2",
          table(`class` := "table-responsive",
            tbody(
              tr(`class` := "summary-line-0", style := "border-top: hidden;",
                td(`class` := "summary-line-0", style := "vertical-align:top;",
                  if (errorAttachments.nonEmpty) {
                    formatAttachments(reportBase, errorAttachments, result.evalStatus.keyword)
                  } else "",
                  " "
                ),
                td(`class` := "summary-line-0",
                  reportPath match {
                    case Some(rpath) =>
                      a(`class` := s"inverted-${cssStatus(reportingStatus.keyword)}", style := s"color: ${linkColor(reportingStatus.keyword)};", href := s"$rpath#${result.evalStatus.keyword}",
                        span(`class` := s"text-${cssStatus(reportingStatus.keyword)}",
                          small(
                            raw(escapeHtml(result.evalStatus.message))
                          )
                        )
                      )
                    case None =>
                      small(
                        raw(escapeHtml(result.evalStatus.message))
                      )
                  }
                )
              )
            )
          )
        )
      } else "",
      td(`class` := "summary-line-2",
        raw(escapeHtml(result.spec.specFile.map(_.getPath()).getOrElse("").toString))
      ),
      td(`class` := "summary-line-2", attr("align") := "right", style := "white-space: nowrap;",
        formatDuration(result.elapsedTime)
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
    StatusKeyword.Passed -> "success", 
    StatusKeyword.Failed -> "danger",
    StatusKeyword.Sustained -> "danger",
    StatusKeyword.Skipped -> "warning",
    StatusKeyword.Pending -> "info",
    StatusKeyword.Loaded -> "success",
    StatusKeyword.Disabled -> "default",
    StatusKeyword.Ignored -> "default"
  )

  private [format] val bgStatus = Map(
    StatusKeyword.Passed -> "success", 
    StatusKeyword.Failed -> "danger",
    StatusKeyword.Sustained -> "danger",
    StatusKeyword.Skipped -> "warning",
    StatusKeyword.Pending -> "info",
    StatusKeyword.Loaded -> "success",
    StatusKeyword.Disabled -> "default",
    StatusKeyword.Ignored -> "success"
  )

  private [format] val linkColor = Map(
    StatusKeyword.Passed -> "#3c763d",
    StatusKeyword.Failed -> "#a94442",
    StatusKeyword.Sustained -> "#a94442",
    StatusKeyword.Skipped -> "#8a6d3b",
    StatusKeyword.Pending -> "#31708f",
    StatusKeyword.Loaded -> "#3c763d",
    StatusKeyword.Disabled -> "grey",
    StatusKeyword.Ignored -> "grey"
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
            img(src := s"${rootPath}resources/img/gwen-logo.png", border := "0", width := "120px")
          )
        ),
        td(
          h3(
            raw(escapeHtml(heading))
          ),
          escapeHtml(path),
          raw("&nbsp;")
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
            a(`class` := s"inverted", id := "failed-link", href := s"#$status", style := "color:white;",
              status.toString
            )
          )
        } else {
          status.toString
        }
      ),
      for {
        opt <- Option(sustainedCount > 0)
        if opt
      } yield {
        Seq(
          span(" "),
          span(`class` := "badge badge-danger",
            if (renderErrorLink) {
              Seq(
                a(`class` := s"inverted", id := "sustained-link", href := s"#$status", style := "color:white;",
                  sustainedError
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

  private [format] def formatAttachments(baseDir: Option[String], attachments: List[(String, File)], status: StatusKeyword): Option[TypedTag[String]] = {
    if (attachments.size > 1) {
      Some(
        formatAttachmentsDropdown("Attachments", baseDir, attachments, status, attachmentHref)
      )
    } else if (attachments.size == 1)  {
      val (name, file) = attachments(0)
      Some(
        a(href := s"${baseDir.map(d => s"$d/").getOrElse("")}${attachmentHref(file)}", target := "_blank", `class` := s"inverted-${cssStatus(status)}",
          strong(style := "font-size: 12px;",
            name
          )
        )
      )
    } else {
      None
    }
  }

  private [format] def formatAttachmentsDropdown(name: String, baseDir: Option[String], attachments: List[(String, File)], status: StatusKeyword, hrefFormatter: File => String): TypedTag[String] = { 
    div(`class` := s"dropdown",
      button(`class` := s"btn btn-${cssStatus(status)} bg-${bgStatus(status)} ${if (status == StatusKeyword.Ignored) "grayed " else ""}dropdown-toggle", attr("type") := "button", attr("data-toggle") := "dropdown", style := "position: relative; top: -0.5px;",
        strong(
          name
        ),
        span(`class` :="caret")
      ),
      ul(`class` := "dropdown-menu pull-right", role := "menu", style := "padding-left:0; max-width: 500px; width: max-content !important;",
        for {
          ((name, file), index) <- attachments.zipWithIndex
        } yield {
          li(role := "presentation", `class` := s"text-${cssStatus(status)}",
            a(`class` := "inverted", role := "menuitem", tabindex := "-1", href := s"${baseDir.map(d => s"$d/").getOrElse("")}${hrefFormatter(file)}", target := "_blank",
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
  }

  private [format] def formatVideoAttachments(reportBase: Option[String], videos: List[File], status: Option[StatusKeyword]): TypedTag[String] = {
    if (videos.size > 1) {
      formatAttachmentsDropdown("Videos", reportBase, videos.map(f => ("Video", f)), status.getOrElse(Disabled.keyword), videoHref)
    } else {
      button(attr("type") := "button", `class` := s"btn btn-${status.map(cssStatus).getOrElse("default")} btn-lg", onclick := s"window.open('${reportBase.map(d => s"$d/").getOrElse("")}${videoHref(videos.head)}', '_blank');", style := "position: relative; top: -1px;",
        "Video"
      )
    }
  }
  
  private [format] def attachmentHref(file: File) = if (FileIO.hasFileExtension("url", file)) Source.fromFile(file).mkString.trim else s"attachments/${file.getName}"
  private [format] def videoHref(file: File) = if (FileIO.hasFileExtension("url", file)) Source.fromFile(file).mkString.trim else s"attachments/videos/${file.getName}"
          
}
