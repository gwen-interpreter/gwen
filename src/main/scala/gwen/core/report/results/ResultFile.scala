/*
 * Copyright 2024 Branko Juric, Brady Wood
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

package gwen.core.report.results

import gwen.core.Errors
import gwen.core.FileIO
import gwen.core.GwenOptions
import gwen.core.GwenSettings
import gwen.core.Settings
import gwen.core.data.CsvDataSource
import gwen.core.status.StatusKeyword

import scala.util.Try

import java.io.File

case class ResultFile(id: String, file: File, scope: Option[ResultScope], status: Option[StatusKeyword], fields: List[ResultField])

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
    val status = Settings.getOpt(s"$fileKey.status").map(StatusKeyword.valueOf)
    val f = Settings.getFile(options.interpolate(s"$fileKey.file"))
    val file = if (f.getParent != null) f else new File(new File(options.reportDir.getOrElse(GwenSettings.`gwen.outDir`), "results"), f.getName)
    val fieldMap = Settings.findAll(k => k.startsWith(s"$fileKey.fields."))
    if (fieldMap.isEmpty) Errors.missingSettingError(s"$fileKey.fields")
    val fieldKeys = fieldMap.keys.toList.map(k => k.substring(0, k.lastIndexOf("."))).distinct.sorted
    val fields = fieldKeys flatMap { key =>
      val fMap = fieldMap.filter((n, _) => n.startsWith(key))
      fMap.map(s => s._1.substring(s._1.lastIndexOf(".") + 1)).foreach(ResultField.validateSettingName)
      val ref = fMap.collectFirst { case (n, v) if n.endsWith(".ref") => (v, options.interpolate(v)) }
      val optional = fMap.collectFirst { case (n, _) if n.endsWith(".optional") => Settings.getBoolean(n) } getOrElse false
      val excludes = fMap.collectFirst { case (n, _) if n.endsWith(".excludes") => Settings.get(n) } map { v => v.split(",").toList.map(_.trim) } getOrElse Nil
      fMap.collectFirst { case (n, v) if n.endsWith(".field") => v } map { 
        f => List(ResultField(f, ref.map(_._2).getOrElse(f), optional)) 
      } getOrElse {
        fMap.iterator.toList.sortBy(_._1).map(_._2).map(v => ResultField(v, v, optional))
      } flatMap { field => 
        if (field.name == "*") {
          val fileOpt = if (field.ref == "") options.dataFile else Some(new File(field.ref))
          fileOpt map { file =>
            if (file.exists) {
              if (FileIO.isCsvFile(file)) {
                CsvDataSource(file).header.map(_.trim).filter(h => !excludes.contains(h)).map(h => ResultField(h, h, field.optional))
              } else if (!ref.map(_._1.contains(s"$$<{${GwenOptions.SettingsKey.dataFile}}>")).getOrElse(false)) {
                Errors.unsupportedFileError(file, "field reference file", "csv")
              } else {
                Nil
              }
            } else Errors.missingFileError(file)

          } getOrElse Nil
        } else {
          List(field)
        }
      }
    }
    fileSettings.map(_._1).filter(s => !s.contains(".fields") && !s.matches(""".*((\.\d+\.)(""" + ResultFieldAtts.values.mkString("|") + "))$")).map(s => s.substring(s.lastIndexOf(".") + 1)).foreach(ResultFile.validateSettingName)
    ResultFile(id, file, scope, status, fields)
  }
}

enum ResultFileAtts:
  case file, scope, status