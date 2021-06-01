/*
 * Copyright 2018-2021 Branko Juric, Brady Wood
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

import org.apache.commons.lang3.StringUtils

import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex
import gwen.core.state.TopScope

/**
  * Can be mixed into evaluation engines to provide template matching support. This will allow users to match any
  * source content with a target template and also extract (@{name}), ignore (!{}), and inject (${name}) attributes
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
    val names = """@\{.+?\}|!\{\}""".r.findAllIn(template).toList.zipWithIndex map { case (n, i) =>
      if (n == "!{}") s"![$i]" else n
    }
    names.groupBy(identity).collectFirst { case (n, vs) if vs.size > 1 =>
      Errors.templateMatchError(s"$n parameter defined ${vs.size} times in template '$template'")
    }

    val tLines = Source.fromString(template).getLines().toList
    val lines = tLines zip Source.fromString(source).getLines().toList

    val values = (lines.zipWithIndex.filter(_._1._1.matches(""".*(@\{.*?\}|!\{.*?\}).*""")) map { case ((ttLine, aLine), idx) =>
      (Regex.quote(ttLine).replaceAll("""@\{\s*\}""", """\{@\}""").replaceAll("""@\{.*?\}|!\{\}""", """\\E(.*?)\\Q""").replaceAll("""\\Q\\E""", "").replaceAll("""!\{.+?\}""", """\{!\}"""), aLine, idx)
    }).flatMap { case (tLine, aLine, idx) =>
      tLine.r.unapplySeq(aLine).getOrElse {
        Errors.templateMatchError(s"Could not match '$aLine' at line ${idx + 1} in $sourceName to '${tLines(idx)}' in template (check literals and/or syntax)")
      }
    }
    val params = names zip values
    val resolved = params.foldLeft(template) { (result, param) =>
      val (n, v) = param
      if (n.matches("""!\[\d+\]""")) result.replaceFirst("""!\{\}""", v)
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