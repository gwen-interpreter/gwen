/*
 * Copyright 2015 Branko Juric, Brady Wood
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
package gwen.eval.support

import gwen._

import com.typesafe.scalalogging.LazyLogging

import scala.util.Try
import scala.util.Success

/**
 * Provides support for string interpolation.
 */
trait InterpolationSupport extends LazyLogging {

  private val propertySyntax = """^(?s)(.*)\$\{(.+?)\}(.*)$""".r
  private val paramSyntax = """^(?s)(.*)\$<(.+?)>(.*)$""".r

  final def interpolate(source: String)(resolve: String => String): String = {
    source match {
      case propertySyntax(prefix, property, suffix) =>
        logger.debug(s"Resolving property-syntax binding: $${$property}")
        val iProperty = interpolate(property) { resolve }
        interpolate(s"$prefix${resolve(iProperty)}$suffix") { resolve }
      case paramSyntax(prefix, param, suffix) =>
        logger.debug(s"Resolving param-syntax binding: $$<$param>")
        val resolved = resolve(s"<${param}>")
        val substitution = if (resolved == s"$$<$param>") s"$$[param:$param]" else resolved
        interpolate(s"$prefix${substitution}$suffix") { resolve }
      case r"""(.+?)$prefix"\s*\+\s*(.+?)$binding\s*\+\s*"(.+?)$suffix""" =>
        logger.debug(s"Resolving concat-syntax binding: + $binding +")
        interpolate(s"$prefix${resolve(binding)}$suffix") { resolve }
      case r"""(.+?)$prefix"\s*\+\s*(.+?)$binding\s*""" =>
        if(binding.contains('"')) {
          source
        } else {
          logger.debug(s"""Resolving concat-syntax binding: "" + $binding""")
          interpolate(s"""$prefix${resolve(binding)}"""") { resolve }
        }
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