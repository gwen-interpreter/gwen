/*
 * Copyright 2018-2024 Branko Juric, Brady Wood
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

import gwen.core._

import scala.util.chaining._

import org.apache.commons.lang3.StringUtils

import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex
import gwen.core.state.TopScope
import gwen.core.state.StateLevel

/**
  * Can be mixed into evaluation engines to provide template matching support. This will allow users to match any
  * source content with a target template and also extract (@{name}), ignore (@{*}), and inject (${name}) attributes
  * in scope.
  */
trait TemplateSupport {

  /**
    * Matches a template against a given source and extracts, ignores, or injects values.
    *
    * @param template the template string
    * @param source the source string
    * @param sourceName the name of the source attribute
    * @return success if there is a match; an error otherwise
    */
  def matchTemplate(template: String, source: String, sourceName: String, topScope: TopScope): Try[Boolean] = Try {
    normaliseTemplate(template, source, sourceName, topScope) match {
      case Nil =>
        Errors.templateMatchError(s"Template mismatch")
      case tLines =>
      val tString = tLines.mkString("\n")
      val names = """@\{.+?\}""".r.findAllIn(tString).toList.zipWithIndex map { case (n, i) =>
        if (n == "@{*}") s"*[$i]" else n
      }
      names.groupBy(identity).collectFirst { case (n, vs) if vs.size > 1 =>
        Errors.templateMatchError(s"$n parameter defined ${vs.size} times in template '$template'")
      }
      val lines = tLines zip Source.fromString(source).getLines().toList

      val values = (lines.zipWithIndex.filter(_._1._1.matches(""".*@\{.*?\}.*""")) map { case ((ttLine, aLine), idx) =>
        (Regex.quote(ttLine).replaceAll("""@\{\s*\}""", """\{@\}""").replaceAll("""@\{.*?\}""", """\\E(.*?)\\Q""").replaceAll("""\\Q\\E""", ""), aLine, idx)
      }).flatMap { case (tLine, aLine, idx) =>
        tLine.r.unapplySeq(aLine).getOrElse {
          Errors.templateMatchError(s"Failed to match '$aLine' at line ${idx + 1} in $sourceName to '${tLines(idx)}' in template")
        }
      }
      val params = names zip values
      val resolved = params.foldLeft(tString) { (result, param) =>
        val (n, v) = param
        if (n.matches("""\*\[\d+\]""")) result.replaceFirst("""@\{\*\}""", v)
        else result.replace(n, v)
      }
      source == resolved tap { isMatch =>
        if (isMatch) {
          params.filter { case (n, _) => n.matches("""@\{.+?\}""") } foreach { case (n, v) =>
            topScope.set(n.substring(2, n.length-1), v) }
        } else {
          val commonPrefix = StringUtils.getCommonPrefix(source, resolved)
          val (line, column) = StringOps.lastPositionIn(source.substring(0, commonPrefix.length + 1))
          val diffChar = source.charAt(commonPrefix.length)
          val diffLine = s"${Source.fromString(commonPrefix).getLines().toList.last}[$diffChar].."
          Errors.templateMatchError(s"Expected '${resolved.charAt(commonPrefix.length)}' but got '$diffChar' at line $line position $column in $sourceName: '$diffLine'")
        }
      }
    }
  }

  private def normaliseTemplate(template: String, source: String, sourceName: String, topScope: TopScope): List[String] = {
    val sIter = source.linesIterator
    val tIter = template.linesIterator
    var tLines: List[String] = Nil
    while (sIter.hasNext) {
      var sLine = sIter.next
      if (tIter.hasNext) {
        var tLine = tIter.next
        if (tLine.trim == "@{*}") tLines = sLine :: tLines
        else if (tLine.trim == "@{**}") {
          tLines = sLine :: tLines
          while(tLine != null && tLine.trim.matches("""@\{\*{1,2}\}""")) {
            if (tIter.hasNext) tLine = tIter.next
            else tLine = null
          }
          if(tLine != null) {
            var stop = false
            while(!stop && sIter.hasNext) { // copy lines from source to template in place of @{**}
              sLine = sIter.next
              stop = Try(matchTemplate(tLine, sLine, sourceName, new TopScope(StateLevel.feature)).isSuccess).getOrElse(false)
              tLines = (if (stop) tLine else sLine) :: tLines 
            }
          }
        } else {
          tLines = tLine :: tLines
        }
      } else {
        tLines = sLine :: tLines
      }
    }
    if (tIter.hasNext) Nil  // not a complete match
    else tLines.reverse
  }
}
