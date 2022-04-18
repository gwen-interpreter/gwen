/*
 * Copyright 2022 Branko Juric, Brady Wood
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

import scala.util.Try

/** 
  * Can be mixed into evaluation contexts to compare two strings for similarity using the 
  * DSC (Dice Similarity Coefficient) algorithm.
  */
trait SimilaritySupport {

  /**
    * Compares two strings for similarity and returns a percentage score.
    * 
    * @param value1 string 1
    * @param value2 string 2
    * @return the DSC similarity score (if it could be calculated), otherwise None
    */
  def dscSimilarity(value1: String, value2: String): Option[Double] = {
    val (x, y) = (pair(value1), pair(value2))
    Option(x ++ y) filter(_.nonEmpty) map { union =>
      2.0 * (x intersect y).size / union.size
    }
  }

  /**
   * Splits a string into adjacent word-letter pairs.
   */
  private def pair(value: String) = value.split("\\s").flatMap(_.sliding(2))

}