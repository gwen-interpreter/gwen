/*
 * Copyright 2026 Branko Juric, Brady Wood
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

package gwen.core.eval.support

import java.time.format.DateTimeFormatter
import java.time.LocalDateTime
import java.time.ZoneId
import java.util.Date
import java.text.DecimalFormat
import java.text.NumberFormat
import java.text.SimpleDateFormat

trait TextFormatSupport {

  private def Ordinals = "(st|nd|rd|th)"

  def formatDateTime(source: String, sourceFormat: String, targetFormat: String): String = {
    format(Some(source), Some(sourceFormat), targetFormat) { (s, sf, tf) => 
      new SimpleDateFormat(sf).parse(s).toInstant.atZone(ZoneId.systemDefault).toLocalDateTime.format(DateTimeFormatter.ofPattern(tf))
    }
  }

  def formatDateTime(date: Date, targetFormat: String): String = {
    format(None, None, targetFormat) { (s, sf, tf) => 
      date.toInstant.atZone(ZoneId.systemDefault).toLocalDateTime.format(DateTimeFormatter.ofPattern(tf))
    }
  }

  def formatNumber(source: String, sourceFormat: String, targetFormat: String): String = {
    format(Some(source), Some(sourceFormat), targetFormat) { (s, sf, tf) => 
      new DecimalFormat(tf).format(DecimalFormat(sf).parse(s))
    }
  }

  def format(source: Option[String], sourceFormat: Option[String], targetFormat: String)(formatter: (String, String, String) => String): String = {
    val (sValue, sFormat) = source flatMap { src =>
      sourceFormat map { srcFormat =>
        splitOrdinal(srcFormat) map { (left, right) => 
          (Some(src.replaceAll(Ordinals, "")), Some(left + right))
        } getOrElse {
          (Some(src), Some(srcFormat))
        }
      }
    } getOrElse (None, None)
    splitOrdinal(targetFormat) map { (left, right) =>
      val leftFormatted = if (left.trim() == "") left else formatter(sValue.orNull, sFormat.orNull, left)
      val rightFormatted = if (right.trim() == "") right else format(sValue, sFormat, right)(formatter)
      leftFormatted + suffix(leftFormatted.trim()) + rightFormatted
    } getOrElse {
      formatter(sValue.orNull, sFormat.orNull, targetFormat)
    }
  }

  private def splitOrdinal(format: String): Option[(String, String)] = {
    val idx = format.indexOf(Ordinals) 
    if (idx != -1) {
      Some((format.substring(0, idx), format.substring(idx + Ordinals.length)))
    } else {
      None
    }
  }

  private def suffix(str: String): String = {
    if (str.endsWith("1") && !str.endsWith("11")) "st"
    else if (str.endsWith("2") && !str.endsWith("12")) "nd"
    else if (str.endsWith("3") && !str.endsWith("13")) "rd"
    else "th"
  }

}
