/*
 * Copyright 2014-2023 Branko Juric, Brady Wood
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

package gwen.core.data

import gwen.core.Interpolator

import java.io.File

/**
  * Captures a data record used to initialise feature level data bindings
  * 
  *  @param dataSource the data source
  *  @param recordNo the current data record number
  *  @param totalRecs the total number of records
  *  @param data the current data  
  */
class DataRecord(val dataSource: DataSource, val recordNo: Int, val totalRecs: Int, val data: List[(String, String)]) {
  val descriptor: String = s"${recordNo} of ${totalRecs}"
  private lazy val interpolator = new Interpolator( name => data.filter(_._1 == name).headOption.map(_._2) )
  def interpolate(source: String): String = interpolator.interpolate(source)
  def interpolateLenient(source: String): String = interpolator.lenient.interpolate(source)
  def interpolateStrict(source: String): String = interpolator.strict.interpolate(source)
  override def toString = s"DataRecord(${dataSource.dataFile.getPath}[$recordNo])"
}

object DataRecord {
  def interpolateLenient(dataRecord: Option[DataRecord])(source: String): String = {
    dataRecord.map(_.interpolateLenient(source)).getOrElse(source)
  }
}
