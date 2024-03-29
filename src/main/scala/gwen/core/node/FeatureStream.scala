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
      val inputs = 
        if (location.getParentFile == null) {
          discoverInputs(location.getAbsoluteFile.getParentFile, (Nil, dataFile))
        } else {
          discoverInputsInPath(location.getParentFile, (Nil, dataFile)) match {
            case (metas, data) => (metas.reverse, data) 
          }
        }
      deepRead(location, inputs._1, inputs._2)
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
      val inputs = discoverInputs(location, (metaFiles, dataFile))
      val files = Option(location.listFiles).getOrElse(Array[File]())
      files.to(LazyList).flatMap(deepRead(_, inputs._1, inputs._2)) 
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
    * Scans for meta and data files in the specified directory.
    * 
    * @param dir the directory to scan in
    * @param inputs tuple of accumulated meta files and one optional data file
    */
  private def discoverInputs(dir: File, inputs: (List[File], Option[File])): (List[File], Option[File]) = {
    val (metaFiles, dataFile) = inputs
    val files = Option(dir.listFiles).getOrElse(Array[File]())
    val metas = if (GwenSettings.`gwen.auto.discover.meta`) files.filter(FileIO.isMetaFile).toList else Nil
    val inputs1 = (metaFiles ::: metas, dataFile)
    val datas = {
      (if (GwenSettings.`gwen.auto.discover.data.csv`) files.filter(FileIO.isCsvFile).toList else Nil) match {
        case Nil => if (GwenSettings.`gwen.auto.discover.data.json`) files.filter(FileIO.isJsonFile).toList else Nil
        case ls => ls
      }
    }
    datas match {
      case Nil => inputs1
      case data :: Nil => (inputs1._1, Some(data))
      case _ => 
        if (dataFile.isEmpty) Errors.ambiguousCaseError(s"Ambiguous: expected 1 data file in ${dir.getName} directory but found ${datas.size}")
        else inputs1
    }
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
    * @param inputs tuple of accumulated meta files and one optional data file
    */
  @tailrec
  private def discoverInputsInPath(dir: File, inputs: (List[File], Option[File])): (List[File], Option[File]) = { 
    val hasParentDir = FileIO.hasParentDirectory(dir) 
    if (!hasParentDir) {
      discoverInputs(dir, inputs)
    } else {
      val parent = dir.getParentFile
      discoverInputsInPath(parent, discoverInputs(dir, inputs))
    }
  }
  
}

