/*
 * Copyright 2020-2024 Branko Juric, Brady Wood
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
  private val MaskedNameSuffix = ":masked"
  private def Mask = GwenSettings.`gwen.mask.char`.toString * 5
  private def MaskPattern = Pattern.quote(Mask)
  private def MaskedNamePattern = s"(.+?)$MaskedNameSuffix".r
  private def MaskedValuePattern = s"(?s).*($MaskPattern$ZeroChar+).*".r

  private def countZeroes(masked: String): Int = masked.count(_ == ZeroChar)

  def isMaskedName(name: String): Boolean = name.endsWith(MaskedNameSuffix)

  def mask(name: String, value: String): String = {
    if (name.startsWith("gwen.") && !name.startsWith("gwen.db")) {
      Errors.unsupportedMaskedPropertyError(s"Masking not supported for gwen.* setting: $name")
    }
    val mValue = MaskedValues.collectFirst {
      case mValue if mValue.name == name && mValue.plain == value => mValue
    } getOrElse {
      MaskedValue(name, value) tap { mValue =>
        MaskedValues += mValue
      }
    }
    mValue.toString
  }

  def parse(name: String, value: String): Option[(String, String)] = {
    name match {
      case MaskedNamePattern(n) =>
        Some((n, mask(name, value)))
      case _ =>
        None
    }
  }

  def maskedValue(name: String): Option[String] = MaskedValues.find(_.name == s"$name$MaskedNameSuffix").map(_.masked)

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
