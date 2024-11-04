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
      val metaFiles = 
        if (location.getParentFile == null) {
          discoverMeta(location.getAbsoluteFile.getParentFile, Nil)
        } else {
          discoverMetaInPath(location.getParentFile, Nil).reverse
        }
      deepRead(location, metaFiles, dataFile)
  }
  
  /**
    * Recursively reads and streams features from a single file system location.
    *
    * @param location a file system location to read
    * @param metaFiles optionally accumulated meta files
    * @param dataFile optional data file (for data driven execution)
    * @return a stream of [FeatureUnit]s found at the location
    */
  private def deepRead(location: File, metaFiles: List[File], dataFile: Option[File]): LazyList[FeatureUnit] = {
    if (FileIO.isDirectory(location)) {
      val metas = discoverMeta(location, metaFiles)
      val files = Option(location.listFiles).getOrElse(Array[File]())
      files.to(LazyList).flatMap(deepRead(_, metas, dataFile)) 
    } else if (FileIO.isFeatureFile(location)) {
      val metas = FileIO.appendFile(metaFiles ++ inputMeta, Settings.UserMeta)
      val unit = FeatureUnit(
        Root,
        location, 
        if (GwenSettings.`gwen.associative.meta`)  {
          applyAssociativeMeta(location, metas)
        } else {
          metas
        },
        None,
        tagFilter)
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
  
  /**
    * Scans for meta files in the specified directory.
    * 
    * @param dir the directory to scan in
    * @param metaFiles list of accumulated meta files
    */
  private def discoverMeta(dir: File, metaFiles: List[File]): List[File] = {
    val files = Option(dir.listFiles).getOrElse(Array[File]())
    val metas = if (GwenSettings.`gwen.auto.discover.meta`) files.filter(FileIO.isMetaFile).toList else Nil
    metaFiles ::: metas
  }

  private def applyAssociativeMeta(featureFile: File, metas: List[File]): List[File] = {
    var associateMeta: Option[File] = None
    // filter out all meta associated with other features files
    val filteredMetas = metas.filter { m => 
      val associatedFeature = new File(m.getParentFile(), s"${m.simpleName}.feature")
      val isAssociate = associatedFeature.isSame(Option(featureFile))
      if (isAssociate) {
        associateMeta = Some(m)
      }
      !isAssociate && !associatedFeature.exists
    }
    // put associate meta at end of list to ensure it is loaded last (so it overrides other meta)
    filteredMetas ++ associateMeta.toList
  }
  
  /**
    * Scans for data and meta files in the parent hierarchy starting from the 
    * given directory.
    * 
    * @param dir the directory to scan from
    * @param metaFiles list of accumulated meta files
    */
  @tailrec
  private def discoverMetaInPath(dir: File, metaFiles: List[File]): List[File] = { 
    val hasParentDir = FileIO.hasParentDirectory(dir) 
    if (!hasParentDir) {
      discoverMeta(dir, metaFiles)
    } else {
      val parent = dir.getParentFile
      discoverMetaInPath(parent, discoverMeta(dir, metaFiles))
    }
  }
  
}

