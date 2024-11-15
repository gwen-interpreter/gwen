/*
 * Copyright 2014-2024 Branko Juric, Brady Wood
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

import gwen.core._
import gwen.core.data.DataSource
import gwen.core.node.gherkin.TagFilter

import scala.annotation.tailrec

import com.typesafe.scalalogging.LazyLogging

import java.io.File

/**
  * Reads and streams individual features and feature suites from the file system.  
  * An individual feature is a single feature file and meta file list pair, 
  * whereas a feature suite is a directory containing one or many feature files, 
  * zero or many meta files, and zero or many sub directories repeating the same 
  * structure. Both individual features and feature suites are streamed as 
  * [FeatureUnit] units. The former yields only one element and the latter 
  * yields many.  
  *  
  * @author Branko Juric
  */
class FeatureStream(inputMeta: List[File], tagFilter: TagFilter) extends LazyLogging {
  
  /**
    * Reads and streams features from multiple file system locations.  
    * 
    * @param locations the list of file system locations to read; each location is either a  
    *        feature file or a directory
    * @param dataFile optional data file (for data driven execution)
    * @return a stream of nested streams (one nested stream per given location)
    */
  def readAll(locations: List[File], dataFile: Option[File]): LazyList[LazyList[FeatureUnit]] = locations.foldLeft(LazyList[LazyList[FeatureUnit]]()) { 
    (suites: LazyList[LazyList[FeatureUnit]], location: File) => {
      suites #::: LazyList(read(location, dataFile)) 
    }
  } 
  
  /**
    * Reads and streams features from a single file system location.
    *
    * @param location a file system location to read
    * @param dataFile optional data file (for data driven execution)
    * @return a stream of [FeatureUnit]s found at the location
    */
  def read(location: File, dataFile: Option[File]): LazyList[FeatureUnit] = {
      deepRead(location, FileIO.appendFile(inputMeta, Settings.UserMeta), dataFile)
  }
  
  /**
    * Recursively reads and streams features from a single file system location.
    *
    * @param location a file system location to read
    * @param metaFiles additional meta files
    * @param dataFile optional data file (for data driven execution)
    * @return a stream of [FeatureUnit]s found at the location
    */
  private def deepRead(location: File, metaFiles: List[File], dataFile: Option[File]): LazyList[FeatureUnit] = {
    if (FileIO.isDirectory(location)) {
      val files = Option(location.listFiles).getOrElse(Array[File]())
      files.to(LazyList).flatMap(deepRead(_, metaFiles, dataFile)) 
    } else if (FileIO.isFeatureFile(location)) {
      val assocMeta = Option(new File(location.getParentFile(), s"${location.dropExtension}.meta")).filter(_.exists)
      val metas = (metaFiles ++ assocMeta.toList).distinct
      val unit = FeatureUnit(Root, location, metas, None, tagFilter)
      dataFile match {
        case Some(file) => new FeatureSet(unit, DataSource(file)).to(LazyList)
        case None =>
          logger.info(s"Found $unit")
          LazyList(unit)
      }
    } else {
      if (!FileIO.isMetaFile(location)) {
        logger.debug(s"Ignoring file: $location")
      }
      LazyList()
    }
  }
  
}
