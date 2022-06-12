/*
 * Copyright 2014-2021 Branko Juric, Brady Wood
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

import java.io.File

/**
  * Captures a data record used to initialise feature level data bindings
  * 
  *  @param dataFile the source data file
  *  @param recordNo the current data record number
  *  @param totalRecs the total number of records
  *  @param data the current data  
  */
class DataRecord(val dataFile: File, val recordNo: Int, val totalRecs: Int, val data: List[(String, String)]) {
  override def toString = s"DataRecord(${dataFile.getPath}[$recordNo])"
  def interpolator: String => String = name => {
    data.filter(_._1 == name).headOption.map(_._2).getOrElse(s"$$[$name]")
  }
  def descriptor: String = s"${recordNo} of ${totalRecs}"

}
