/*
 * Copyright 2024-2026 Branko Juric, Brady Wood
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

package gwen.core.result

import gwen.core._
import gwen.core.data.CsvDataSource
import gwen.core.node.gherkin.Tag
import gwen.core.node.gherkin.Annotations
import gwen.core.node.GwenNode
import gwen.core.node.NodeType
import gwen.core.report.ReportFormat
import gwen.core.state.Environment
import gwen.core.status.StatusKeyword

import scala.util.Try

import java.io.File
import gwen.core.state.SensitiveData

case class ResultFile(id: String, file: File, scope: Option[ResultScope], status: Option[StatusKeyword], fields: List[ResultField]) {

  def logRecord(node: GwenNode, env: Environment, options: GwenOptions): Boolean = {
    val resultsEnabled = options.reportFormats.contains(ReportFormat.results)
    val nodeTypeOK = scope.map(_.nodeType == node.nodeType).getOrElse(true)
    val nodeNameOK = scope.map(_.nodeName.map(n => node.name.matches("(.* -- )?" + n + "( -- .*)?$")).getOrElse(true)).getOrElse(true)
    val statusOK = status.map(_ == node.evalStatus.keyword).getOrElse(true)
    if (resultsEnabled && nodeTypeOK && nodeNameOK && statusOK) {
      var errors: List[String] = Nil
      val record = fields map { field =>
        val value = {
          Try(env.getBoundValue(field.ref)) getOrElse {
            env.topScope.getOpt(field.ref) getOrElse {
              field.defaultValue getOrElse {
                errors = (errors ++ List(s"Unbound ${field.name} field${ if (field.name != field.ref) s" reference: ${field.ref}" else ""} in ${file} results file id ${id}"))
                s"Unbound ref: ${field.ref}"
              }
            }
          }
        }
        val v = if (FileIO.isCsvFile(file)) formatCSV(value)
        else value
        if (field.unmask) SensitiveData.withValue(v)(identity) else v
      }
      if (errors.nonEmpty) {
        Errors.resultsFileErrors(errors)
      }
      this.synchronized {
        file.appendLine(record.mkString(","))
        if (FileIO.isCsvFile(file)) {
          fields.find(_.sort.nonEmpty) foreach { sortField => 
            val fieldName = sortField.name
            val asending = sortField.sort.exists(_ == SortOrder.ascending)
            CsvDataSource(file).table match {
              case header::records =>
                header.zipWithIndex.find(_._1 == fieldName).map(_._2) foreach { sortFieldIndex => 
                  val numeric = records.forall(rec => Try(rec(sortFieldIndex).toDouble).isSuccess)
                  val sorted = records.sortWith { (a, b) =>
                    val aValue = a(sortFieldIndex)
                    val bValue = b(sortFieldIndex)
                    val compareResult = if (numeric) aValue.toDouble.compareTo(bValue.toDouble) else aValue.compareTo(bValue)
                    if (asending) compareResult < 0 else compareResult > 0
                  }
                  file.writeLine(header.map(formatCSV).mkString(","))
                  sorted foreach {record =>
                    file.appendLine(record.map(formatCSV).mkString(","))
                  }
                }
              case _ =>
            }
          }
        }
      }
      true
    } else {
      false
    }
  }

  private def formatCSV(value: String): String = {
    if (value.trim != "") Formatting.escapeCSV(Formatting.escapeNewLineChars(value))
    else value
  }

}

object ResultFile {

  val SettingsKey = "gwen.report.results.files"
  
  private def validateSettingName(name: String): Unit = {
    Try(ResultFileAtts.valueOf(name)) getOrElse {
      Errors.illegalSettingAttributeError(name, "gwen.report.results.files", ResultFileAtts.values.mkString(", "))
    }
  }

  def apply(id: String, fileSettings: Map[String, String], options: GwenOptions): ResultFile = {
    val fileKey = s"${ResultFile.SettingsKey}.$id"
    val scope = ResultScope.resolve(s"$fileKey.scope")
    val status = fileSettings.get(s"$fileKey.status").map(StatusKeyword.valueOf)
    val fSetting = options.interpolate(s"$fileKey.file")
    val f = fileSettings.get(fSetting) map { path => new File(path) } getOrElse {
      Errors.missingSettingError(fSetting)
    }
    val file = if (f.getParent != null) f else new File(new File(options.reportDir.getOrElse(GwenSettings.`gwen.outDir`), "results"), f.getName)
    val fieldSettings = fileSettings.filter(_._1.startsWith(s"$fileKey.fields."))
    if (fieldSettings.isEmpty) Errors.missingSettingError(s"$fileKey.fields")
    val fieldKeys = fieldSettings.keys.toList.map(k => k.substring(0, k.lastIndexOf("."))).distinct.sorted
    val fields = fieldKeys flatMap { key =>
      val fMap = fieldSettings.filter((n, _) => n.startsWith(key))
      fMap.map(s => s._1.substring(s._1.lastIndexOf(".") + 1)).foreach(ResultField.validateSettingName)
      val ref = fMap.collectFirst { case (n, v) if n.endsWith(".ref") => (v, options.interpolate(v)) }
      val defaultValue = fMap.collectFirst { case (n, _) if n.endsWith(".defaultValue") => fileSettings(n) }
      val unmask = fMap.collectFirst { case (n, _) if n.endsWith(".unmask") => fileSettings(n).toBoolean } getOrElse false
      val sort = fMap.collectFirst { case (n, _) if n.endsWith(".sort") => 
        val v = fileSettings(n)
        Try(SortOrder.valueOf(v)).map(Some(_)) getOrElse {
          Errors.illegalSettingError(n, v, SortOrder.values.mkString(", "))
        }
      } getOrElse None
      val excludes = fMap.collectFirst { case (n, _) if n.endsWith(".excludes") => fileSettings(n) } map { v => v.split(",").toList.map(_.trim) } getOrElse Nil
      fMap.collectFirst { case (n, v) if n.endsWith(".field") => v } map { 
        f => List(ResultField(f, ref.map(_._2).getOrElse(f), defaultValue, unmask, sort)) 
      } getOrElse {
        fMap.iterator.toList.sortBy(_._1).map(_._2).map(v => ResultField(v, v, defaultValue, unmask, sort))
      } flatMap { field => 
        if (field.name == "*") {
          val fileOpt = if (field.ref == "") options.dataFile else Some(new File(field.ref))
          fileOpt map { file =>
            if (file.exists) {
              if (FileIO.isCsvFile(file)) {
                CsvDataSource(file).header.map(_.trim).filter(h => !excludes.contains(h)).map(h => ResultField(h, h, field.defaultValue, field.unmask, field.sort))
              } else if (!ref.map(_._1.contains(s"$$<{${GwenOptions.SettingsKey.dataFile}}>")).getOrElse(false)) {
                Errors.unsupportedFileError(file, "field reference file", "csv")
              } else {
                Nil
              }
            } else Errors.missingFileError("Reference file", file)

          } getOrElse Nil
        } else {
          List(field)
        }
      }
    }
    fileSettings.map(_._1).filter(s => !s.contains(".fields") && !s.matches(""".*((\.\d+\.)(""" + ResultFieldAtts.values.mkString("|") + "))$")).map(s => s.substring(s.lastIndexOf(".") + 1)).foreach(ResultFile.validateSettingName)
    val sortFields = fields.filter(_.sort.nonEmpty)
    if (sortFields.size > 1) {
      Errors.multipleSortFieldsError(fileKey, sortFields.flatMap(_.name).mkString(", "))
    }
    ResultFile(id, file, scope, status, fields)
  }

  def parseAnnotation(tags: List[Tag], resultFiles: List[ResultFile], nodeType: NodeType): List[String] = {
    tags.filter(_.name.startsWith(Annotations.Results.toString)) flatMap { annotation => 
      annotation.toString match {
        case r"""@Results\((.+?)$value\)""" =>
          Tag.parseListValue(annotation.sourceRef, Annotations.Results, None, value) map { id => 
            resultFiles.find(_.id == id) match {
              case Some(resFile) =>
                if (resFile.scope.nonEmpty) {
                  Errors.resultsFileError(s"Scope not permitted on $id results file when logging with $annotation annotation${Errors.at(annotation.sourceRef)} - (remove gwen.report.results.files.$id.scope setting or don't use annotation)")
                } else {
                  id
                }
              case None =>
                Errors.resultsFileError(s"Results file not found with id: $id - check your gwen.report.results.files.$id setting or $annotation annotation${Errors.at(annotation.sourceRef)}")
            }
          }
        case _ => Errors.invalidTagError(s"""Invalid Results annotation: $annotation${Errors.at(annotation.sourceRef)} - correct syntax is @Results('id') or @Results(['id1','id2','idN'])""")
      }
    }
  }
}

enum ResultFileAtts:
  case file, scope, status