package gwen.dsl
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
import scala.concurrent.duration._
import java.text.DecimalFormat

/**
 * Formats durations for presentation purposes.
 */
object DurationFormatter {
  
  private val Formatters = List(
    HOURS -> ("h", new DecimalFormat("00")),
    MINUTES -> ("m", new DecimalFormat("00")),
    SECONDS -> ("s", new DecimalFormat("00")),
    MILLISECONDS -> ("ms", new DecimalFormat("000"))
  )

  /**
   * Formats a given duration to ##h ##m ##s ###ms format.
   * 
   * @param duration
   * 			the duration to format
   */
  def format(duration: Duration): String = {
    val nanos = duration.toNanos
    val msecs = (nanos / 1000000) + (if ((nanos % 1000000) < 500000) 0 else 1)
    if (msecs > 0) {
      var duration = Duration(msecs, MILLISECONDS)
      Formatters.foldLeft("") { (acc: String, f: (TimeUnit, (String, DecimalFormat))) =>
        val (unit, (unitName, formatter)) = f
        val unitValue = duration.toUnit(unit).toLong
        if (acc.length() == 0 && unitValue == 0) "" 
        else {
          duration = duration - Duration(unitValue, unit)
          s"$acc ${formatter.format(unitValue)}$unitName"
        }
      }.trim.replaceFirst("^0+(?!$)", "")
    } else ""
  }
  
}