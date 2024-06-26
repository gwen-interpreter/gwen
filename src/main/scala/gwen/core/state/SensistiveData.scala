/*
 * Copyright 2020-2021 Branko Juric, Brady Wood
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

package gwen.core.state

import gwen.core.Errors
import gwen.core.Formatting
import gwen.core.GwenSettings

import scala.collection.mutable
import scala.util.chaining._

import java.util.concurrent.atomic.AtomicInteger
import java.util.regex.Pattern

/**
  * Object for dealing with sensitive data with overriden toString to
  * prevent logging and reporting.
  */
object SensitiveData {

  private val ZeroChar = Formatting.ZeroChar // zero width space char

  private sealed case class MaskedValue(name: String, plain: String) {
    val masked = s"$Mask${ZeroChar.toString * ZeroCounter.incrementAndGet()}"
    override final def toString = masked
  }

  private val MaskedValues = mutable.ArrayBuffer[MaskedValue]()

  private val ZeroCounter = new AtomicInteger(0)
  private val Mask = GwenSettings.`gwen.mask.char`.toString * 5
  private val MaskPattern = Pattern.quote(Mask)
  private val MaskedNameSuffix = ":masked"
  private val MaskedNamePattern = s"(.+?)$MaskedNameSuffix".r
  private val MaskedValuePattern = s"(?s).*($MaskPattern$ZeroChar+).*".r

  private def countZeroes(masked: String): Int = masked.count(_ == ZeroChar)

  def isMaskedName(name: String): Boolean = name.endsWith(MaskedNameSuffix)

  def mask(name: String, value: String): String = {
    val n = s"$name$MaskedNameSuffix"
    if (n.startsWith("gwen.") && !n.startsWith("gwen.db")) {
      Errors.unsupportedMaskedPropertyError(s"Masking not supported for gwen.* setting: $n")
    }
    val mValue = MaskedValues.collectFirst {
      case mValue if mValue.name == n && mValue.plain == value => mValue
    } getOrElse {
      MaskedValue(n, value) tap { mValue =>
        MaskedValues += mValue
      }
    }
    mValue.toString
  }

  def parse(name: String, value: String): Option[(String, String)] = Some {
    name match {
      case MaskedNamePattern(n) =>
        (n, mask(name, value))
      case _ =>
        (name, value)
    }
  }

  // Unmasks the given value and applies the given function to it
  def withValue[T](value: String)(apply: String => T): T = {
    apply(
      if (MaskedValues.nonEmpty && value.contains(ZeroChar)) unmask(value) else value
    )
  }

  private def unmask[T](value: String): String = {
    value match {
      case MaskedValuePattern(masked) =>
        val zeroCount = countZeroes(masked)
        val index = zeroCount - 1
        if (index < MaskedValues.size) {
          val mValue = MaskedValues(index)
          val pattern = s"$MaskPattern$ZeroChar{$zeroCount}"
          unmask(value.replaceAll(pattern, mValue.plain))
        } else {
          value
        }
      case _ => value
    }
  }

}
