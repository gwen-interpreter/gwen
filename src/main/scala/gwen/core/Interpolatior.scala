/*
 * Copyright 2015-2022 Branko Juric, Brady Wood
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

import gwen.core.data.DataSource

import com.typesafe.scalalogging.LazyLogging

object Interpolator {

  val propertySyntax = """^(?s)(.*)\$\{(.+?)\}(.*)$""".r
  val paramSyntax = """^(?s)(.*)\$<(.+?)>(.*)$""".r

  val unresolvedPropertySyntax = """^(?s)(.*)\$\!\{(.+?)\}(.*)$""".r
  val unresolvedParamSyntax = """^(?s)(.*)\$\!<(.+?)>(.*)$""".r

  val reservedPlaeholder: String => Boolean = DataSource.hasLookupPrefix
}

/**
 * Provides support for string interpolation.
 */
class Interpolator(resolver: String => Option[String]) extends LazyLogging {

  /** Resolves all ${property} and $<param> references in the given string.*/
  def interpolate(source: String): String = {
    source match {
      case Interpolator.propertySyntax(prefix, property, suffix) =>
        logger.debug(s"Resolving property-syntax binding: $${$property}")
        interpolate(s"$prefix${resolveProperty(interpolate(property))}$suffix")
      case _ => interpolateParameters(source, interpolate)
    }
  }

  /** Resolves all $<param> references in the given string.*/
  def interpolateParams(source: String): String = interpolateParameters(source, interpolateParams)

  private def interpolateParameters(source: String, interpolator: String => String): String = {
    source match {
      case Interpolator.paramSyntax(prefix, param, suffix) =>
        logger.debug(s"Resolving param-syntax binding: $$<$param>")
        interpolator.apply(s"$prefix${resolveParam(s"<${interpolator.apply(param)}>")}$suffix")
      case _ => restoreUnresolved(source)
    }
  }

  private[core] def resolveProperty(name: String): String = {
    Option(name).filterNot(Interpolator.reservedPlaeholder).map(resolveStrict) getOrElse {
      resolveLenient(name)
    }
  }
  private[core] def resolveParam(name: String): String = resolveStrict(name)

  private final def resolveStrict(name: String): String = {
    resolver.apply(name) getOrElse { Errors.unboundAttributeError(name) }
  }

  private final def resolveLenient(name: String): String = {
    resolver.apply(name) getOrElse { 
      if (name.startsWith("<") && name.endsWith(">")) s"$$!$name"
      else s"$$!{$name}"
    }
  }

  private def restoreUnresolved(source: String): String = {
    source match {
      case Interpolator.unresolvedPropertySyntax(prefix, property, suffix) => 
        restoreUnresolved(s"$prefix$${$property}$suffix")
      case Interpolator.unresolvedParamSyntax(prefix, param, suffix) => 
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

}