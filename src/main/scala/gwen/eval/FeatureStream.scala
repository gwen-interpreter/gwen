/*
 * Copyright 2014-2015 Branko Juric, Brady Wood
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

package gwen.eval

import java.io.File
import scala.Stream
import scala.annotation.tailrec
import scala.collection.immutable.Stream.consWrapper
import gwen.Predefs.Kestrel
import com.typesafe.scalalogging.slf4j.LazyLogging
import gwen.errors._

/**
  * Reads and streams individual features and feature suites from the file system.  
  * An individual feature is a single feature file and optional meta file pair, 
  * whereas a feature suite is a directory containing one or many feature files, 
  * an optional meta file, and zero or many sub directories repeating the same 
  * structure. Both individual features and feature suites are streamed as 
  * [FeatureUnit] units. The former yields only one element and the latter 
  * yields many.  
  *  
  * @author Branko Juric
  */
object FeatureStream extends LazyLogging {
  
  /**
    * Reads and streams features from multiple file system locations.  
    * 
    * @param locations the list of file system locations to read; each location is either a  
    *        feature file or a directory
    * @param dataFile optional data file (for data driven execution)
    * @return a stream of nested streams (one nested stream per given location)
    */
  def readAll(locations: List[File], dataFile: Option[File]): Stream[Stream[FeatureUnit]] = locations.foldLeft(Stream[Stream[FeatureUnit]]()) { 
    (suites: Stream[Stream[FeatureUnit]], location: File) => {
      suites #::: Stream(read(location, dataFile)) 
    }
  } 
  
  /**
    * Reads and streams features from a single file system location.
    *
    * @param location a file system location to read
    * @param dataFile optional data file (for data driven execution)
    * @return a stream of [FeatureUnit]s found at the location
    */
  def read(location: File, dataFile: Option[File]): Stream[FeatureUnit] = {
      val inputs = 
        if (location.getParentFile() == null) {
          discoverInputs(location.getAbsoluteFile().getParentFile(), (Nil, dataFile))
        } else {
          discoverInputsInPath(location.getParentFile(), (Nil, dataFile)) match { 
            case (metas, data) => (metas.reverse, data) 
          }
        }
      deepRead(location, inputs._1, inputs._2)
  }
  
  /**
    * Recursively reads and streams features from a single file system location.
    *
    * @param location a file system location to read
    * @param metaFiles optionally accumulated meta files (default is Nil)
    * @param dataFile optional data file (for data driven execution)
    * @return a stream of [FeatureUnit]s found at the location
    */
  private def deepRead(location: File, metaFiles: List[File] = Nil, dataFile: Option[File]): Stream[FeatureUnit] = {
    if (isDirectory(location)) {
      val inputs = discoverInputs(location, (metaFiles, dataFile))
      location.listFiles().toStream.flatMap(deepRead(_, inputs._1, inputs._2)) 
    } else if (isFeatureFile(location)) {
      val unit = new FeatureUnit(location, metaFiles, None)
      dataFile match {
        case Some(file) => new FeatureSet(unit, file).toStream
        case None =>
          logger.info(s"Found $unit")
          Stream(unit)
      }
    } else {
      if (!isMetaFile(location)) {
        logger.debug(s"Ignoring file: $location")
      }
      Stream()
    }
  }
  
  /**
    * Scans for a meta and data files in the specified directory.
    * 
    * @param dir the directory to scan for meta
    * @param inputs tuple of accumulated meta files and one optional data file
    * @throws gwen.errors.AmbiguousCaseException if more than one meta or data 
    *         file is found in the given directory or more than one data file is 
    *         found in the directory path.
    */
  private def discoverInputs(dir: File, inputs: (List[File], Option[File])): (List[File], Option[File]) = {
    val (metaFiles, dataFile) = inputs
    val metas = dir.listFiles.filter(isMetaFile).toList
    val inputs1 = metas match {
      case metaFile :: Nil => (metaFiles ::: List(metaFile), dataFile)
      case _ :: _ => 
        ambiguousCaseError(s"Ambiguous: expected 1 meta feature in ${dir.getName()} but found ${metas.size}")
      case _ => (metaFiles, dataFile)
    }
    val datas = dir.listFiles.filter(isDataFile).toList
    datas match {
      case Nil => inputs1
      case data :: Nil if (dataFile.isEmpty || dataFile.get.getCanonicalPath().equals(data.getCanonicalPath())) => (inputs1._1, Some(data))
      case _ => 
        dataFile match { 
          case Some(data) =>
            ambiguousCaseError(s"Ambiguous: found data file(s) in ${dir.getName()} path but already got data file ${data.getPath}")
           case _ => ambiguousCaseError(s"Ambiguous: expected 1 data file in ${dir.getName()} path but found ${datas.size}")
        }
       }
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
    val hasParentDir = hasParentDirectory(dir) 
    if (!hasParentDir) {
      discoverInputs(dir, inputs)
    } else {
      val parent = dir.getParentFile
      discoverInputsInPath(parent, discoverInputs(dir, inputs))
    }
  }
  
  private def isDirectory(location: File): Boolean = location != null && location.isDirectory()
  private def hasParentDirectory(location: File): Boolean = location != null && isDirectory(location.getParentFile())
  private def isFeatureFile(file: File): Boolean = hasFileExtension("feature", file)
  private def isMetaFile(file: File): Boolean = hasFileExtension("meta", file)
  private def isDataFile(file: File): Boolean = hasFileExtension("csv", file)
  private def hasFileExtension(extension: String, file: File): Boolean = file.isFile && file.getName().endsWith(s".${extension}")
}

