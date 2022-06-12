/*
 * Copyright 2015-2021 Branko Juric, Brady Wood
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

package gwen.core.node

import gwen.core.state.DataRecord

import scala.util.chaining._

import com.github.tototoshi.csv.CSVReader
import com.github.tototoshi.csv.defaultCSVFormat
import com.typesafe.scalalogging.LazyLogging

import java.io.File

/**
 * Returns an iteration of feature units for each entry in a given
 * data file.
 */
class FeatureSet(unit: FeatureUnit, dataFile: File) extends Iterator[FeatureUnit] with LazyLogging  {

  private val totalRecs = CSVReader.open(dataFile).all().size - 1
  private val dataFeed = CSVReader.open(dataFile).iterator.zipWithIndex
  private val headers = if (dataFeed.hasNext) dataFeed.next()._1 else Nil

  /** Checks if there are more records in the data feed. */
  override def hasNext: Boolean = dataFeed.hasNext

  /** Creates a new feature unit for the next record of data. */
  override def next(): FeatureUnit = {
    val (values, index) = dataFeed.next()
    val data = headers zip values
    val dataRecord = new DataRecord(dataFile, index, totalRecs, data.toList)
    logger.debug(s"$dataRecord: $data")
    FeatureUnit(Root, unit.featureFile, unit.metaFiles, Some(dataRecord), unit.tagFilter) tap { unit =>
      logger.info(s"Mapped $unit")
    }
  }

}
