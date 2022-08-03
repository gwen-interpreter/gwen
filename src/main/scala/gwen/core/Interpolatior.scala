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

import com.typesafe.scalalogging.LazyLogging

import scala.util.Try
import scala.util.Success

/**
 * Provides support for string interpolation.
 */
trait Interpolator extends LazyLogging {

  private val propertySyntax = """^(?s)(.*)\$\{(.+?)\}(.*)$""".r
  private val unresolvedPropertySyntax = """^(?s)(.*)\$\!\{(.+?)\}(.*)$""".r
  private val paramSyntax = """^(?s)(.*)\$<(.+?)>(.*)$""".r

  final def interpolateString(source: String)(resolve: String => String): String = interpolateString(source, false) { resolve }
  final def interpolateStringPreserveUnresolved(source: String)(resolve: String => String): String = interpolateString(source, true) { resolve }
  
  private final def interpolateString(source: String, preserveUnresolved: Boolean)(resolve: String => String): String = {
    source match {
      case propertySyntax(prefix, property, suffix) =>
        logger.debug(s"Resolving property-syntax binding: $${$property}")
        val iProperty = interpolateString(property, preserveUnresolved) { resolve }
        val resolved = resolve(iProperty)
        interpolateString(s"$prefix${if (preserveUnresolved && resolved == iProperty) s"$$!{$property}" else resolved}$suffix", preserveUnresolved) { resolve }
      case paramSyntax(prefix, param, suffix) =>
        logger.debug(s"Resolving param-syntax binding: $$<$param>")
        val resolved = resolve(s"<${param}>")
        val substitution = if (resolved == s"$$<$param>") s"$$[param:$param]" else resolved
        interpolateString(s"$prefix${substitution}$suffix", preserveUnresolved) { resolve }
      case _ => 
        if (preserveUnresolved) restoreUnresolved(source)
        else source
    }
  }

  private def restoreUnresolved(source: String): String = {
    source match {
      case unresolvedPropertySyntax(prefix, property, suffix) => 
        restoreUnresolved(s"$prefix$${$property}$suffix")
      case _ => source
    }
  }

  final def interpolateParams(source: String)(resolve: String => String): String = {
    source match {
      case paramSyntax(prefix, param, suffix) =>
        logger.debug(s"Resolving param-syntax binding: $$<$param>")
        Try(resolve(s"<${param}>")) match {
          case Success(resolved) => 
            val substitution = if (resolved == s"$$<$param>") s"$$[param:$param]" else resolved
            interpolateParams(s"$prefix${substitution}$suffix") { resolve }
          case _ =>
            s"${interpolateParams(prefix)(resolve)}$$<$param>${interpolateParams(suffix)(resolve)}"
        }
        
      case _ => source
    } 
  }

}