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
import java.time.LocalDate
import java.text.DecimalFormat
import java.text.NumberFormat

trait TextFormatSupport {

  private def Ordinals = "(st|nd|rd|th)"

  def formatDateTime(source: String, sourceFormat: String, targetFormat: String): String = {
    format(source, sourceFormat, targetFormat) { (s, sf, tf) => 
      LocalDate.parse(s, DateTimeFormatter.ofPattern(sf)).format(DateTimeFormatter.ofPattern(tf))
    }
  }

  def formatNumber(source: String, sourceFormat: String, targetFormat: String): String = {
    format(source, sourceFormat, targetFormat) { (s, sf, tf) => 
      new DecimalFormat(tf).format(DecimalFormat(sf).parse(s))
    }
  }

  def format(source: String, sourceFormat: String, targetFormat: String)(formatter: (String, String, String) => String): String = {
    val (sValue, sFormat) = splitOrdinal(sourceFormat) map { (left, right) => 
      (source.replaceAll(Ordinals, ""), left + right)
    } getOrElse {
      (source, sourceFormat)
    }
    splitOrdinal(targetFormat) map { (left, right) =>
      val leftFormatted = if (left.trim() == "") left else formatter(sValue, sFormat, left)
      val rightFormatted = if (right.trim() == "") right else format(sValue, sFormat, right)(formatter)
      leftFormatted + suffix(leftFormatted.trim()) + rightFormatted
    } getOrElse {
      formatter(sValue, sFormat,targetFormat)
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
