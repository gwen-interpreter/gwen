/*
 * Copyright 2015-2024 Branko Juric, Brady Wood
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
package gwen.core

import gwen.core.Errors.UnboundReferenceException
import gwen.core.data.DataSource

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.lang3.StringUtils

import scala.util.Try
import scala.util.Success
import scala.util.Failure

object Interpolator {

  val propertySyntax = """^(?s)(.*)\$\{(.+?)\}(.*)$""".r
  val paramSyntax = """^(?s)(.*)\$<(.+?)>(.*)$""".r

  val unresolvedPropertySyntax = """^(?s)(.*)\$\!\{(.+?)\}(.*)$""".r
  val unresolvedParamSyntax = """^(?s)(.*)\$\!<(.+?)>(.*)$""".r

  val isReservedPlaeholder: String => Boolean = DataSource.hasLookupPrefix
}

/**
 * Provides support for string interpolation.
 */
class Interpolator(resolver: String => Option[String]) extends LazyLogging {

  /** Resolves all ${property} and $<param> references in the given string.*/
  def interpolate(source: String): String = {
    source match {
      case Interpolator.propertySyntax(prefix, p, s) =>
        val (property, suffix) = balance('{', '}', p, s)
        logger.debug(s"Resolving property-syntax binding: $${$property}")
        interpolate(s"$prefix${resolveProperty(interpolate(property))}$suffix")
      case _ => interpolateParameters(source, interpolate)
    }
  }

  /** Resolves all $<param> references in the given string.*/
  def interpolateParams(source: String): String = interpolateParameters(source, interpolateParams)

  private def interpolateParameters(source: String, interpolator: String => String): String = {
    source match {
      case Interpolator.paramSyntax(prefix, p, s) =>
        val (param, suffix) = balance('<', '>', p, s)
        logger.debug(s"Resolving param-syntax binding: $$<$param>")
        interpolator.apply(s"$prefix${resolveParam(s"<${interpolator.apply(param)}>")}$suffix")
      case _ => restoreUnresolved(source)
    }
  }

  private[core] def resolveProperty(name: String): String = {
    Option(name).filterNot(Interpolator.isReservedPlaeholder).map(resolveStrict) getOrElse {
      resolveLenient(name)
    }
  }
  private[core] def resolveParam(name: String): String = resolveStrict(name)

  private final def resolveStrict(name: String): String = {
    name match {
      case r"""(.+?)$n \?: (.+)$v""" if !n.contains("=>") =>
        try {
          resolveStrict(n)
        } catch {
          case e: UnboundReferenceException =>
            v match {
              case r"""('|"|`)$q(.*)$qv\1""" => qv
              case _ => Try(ValueLiteral.valueOf(v).value).getOrElse(v)
            }
        }
      case _ =>
        resolver.apply(name) getOrElse { Errors.unboundReferenceError(name) }
    }
  }

  private final def resolveLenient(name: String): String = {
    Try(resolveStrict(name)) match {
      case Success(value) => Option(value)
      case Failure(e) => 
        if (e.isInstanceOf[UnboundReferenceException]) None
        else throw e
    } getOrElse { 
      if (name.startsWith("<") && name.endsWith(">")) s"$$!$name"
      else s"$$!{$name}"
    }
  }

  def restoreUnresolved(source: String): String = {
    source match {
      case Interpolator.unresolvedPropertySyntax(prefix, p, s) => 
        val (property, suffix) = balance('{', '}', p, s)
        restoreUnresolved(s"$prefix$${$property}$suffix")
      case Interpolator.unresolvedParamSyntax(prefix, p, s) => 
        val (param, suffix) = balance('<', '>', p, s)
        restoreUnresolved(s"$prefix$$<$param>$suffix")
      case _ => source
    }
  }

  lazy val lenient: Interpolator = new Interpolator(resolver) {
    override private[core] def resolveProperty(name: String): String = resolveLenient(name)
    override private[core] def resolveParam(name: String): String = resolveLenient(name)
  }

  lazy val strict: Interpolator = new Interpolator(resolver) {
    override private[core] def resolveProperty(name: String): String = resolveStrict(name)
    override private[core] def resolveParam(name: String): String = resolveStrict(name)
  }

  lazy val settings: Interpolator = new Interpolator(resolver) {
    override private[core] def resolveProperty(name: String): String = resolveStrict(name)
    override private[core] def resolveParam(name: String): String = resolveLenient(name)
  }

  private def balance(opening: Char, closing: Char, current: String, suffix: String): (String, String) = {
    val bal = current.count(_ == opening) - current.count(_ == closing)
    if (bal > 0) {
      val closingBraceIndex = StringUtils.ordinalIndexOf(suffix, closing.toString, bal)
      if (closingBraceIndex > -1) {
        val p = suffix.substring(0, closingBraceIndex + 1)  
        val s = suffix.drop(p.length)
        (current + p.trim, s)
      } else {
        (current, suffix)
      }
    } else {
      (current, suffix)
    }
  }

}