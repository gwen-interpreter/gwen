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
    * @return a stream of nested streams (one nested stream per given location)
    */
  def readAll(locations: List[File]): Stream[Stream[FeatureUnit]] = locations.foldLeft(Stream[Stream[FeatureUnit]]()) { 
    (suites: Stream[Stream[FeatureUnit]], location: File) => {
      suites #::: Stream(read(location)) 
    }
  } 
  
  /**
    * Reads and streams features from a single file system location.
    *
    * @param location a file system location to read
    * @return a stream of [FeatureUnit]s found at the location
    */
  def read(location: File): Stream[FeatureUnit] = {
      val metas = 
        if (location.getParentFile() == null) {
          accumulateMeta(location.getAbsoluteFile().getParentFile(), Nil)
        } else {
          accumulateParentMeta(location.getParentFile(), Nil).reverse
        }
      deepRead(location, metas)
  }
  
  /**
    * Recursively reads and streams features from a single file system location.
    *
    * @param location a file system location to read
    * @param metaFiles optionally accumulated meta files (default is Nil)
    * @return a stream of [FeatureUnit]s found at the location
    */
  private def deepRead(location: File, metaFiles: List[File] = Nil): Stream[FeatureUnit] = {
    if (isDirectory(location)) {
      val metas = accumulateMeta(location, metaFiles)
      location.listFiles().toStream.flatMap(deepRead(_, metas)) 
    } else if (isFeatureFile(location)) {
      Stream(new FeatureUnit(location, metaFiles) tap { unit =>
        logger.info(s"Found $unit")
      }) 
    } else {
      if (!isMetaFile(location)) {
        logger.debug(s"Ignoring file: $location")
      }
      Stream()
    }
  }
  
  /**
    * Scans for a meta file in the specified directory and appends it to the 
    * currently accumulated list of meta files if found. An error is thrown if more 
    * than one meta file is found in the specifid directory.
    * 
    * @param dir the directory to scan for meta
    * @param metaFiles the currently accumulated list of meta files
    * @throws gwen.errors.AmbiguousCaseException if more than one meta file is found
    *         in the given directory
    */
  private def accumulateMeta(dir: File, metaFiles: List[File]): List[File] = { 
    val metas = dir.listFiles.filter(isMetaFile).toList
    metas match {
      case metaFile :: Nil =>
        metaFiles ::: List(metaFile)
      case _ :: _ => 
        ambiguousCaseError(s"Ambiguous: expected 1 meta feature in ${dir.getName()} directory but found ${metas.size}")
      case _ => metaFiles
    }
  }
  
  /**
    * Scans for a meta file up the parent hierarchy starting from the given directory.
    * 
    * @param dir the directory to scan for meta from
    * @param metaFiles the currently accumulated list of meta files
    */
  @tailrec
  private def accumulateParentMeta(dir: File, metaFiles: List[File]): List[File] = { 
    val hasParentDir = hasParentDirectory(dir) 
    if (!hasParentDir) {
      accumulateMeta(dir, metaFiles)
    } else {
      val parent = dir.getParentFile
      accumulateParentMeta(parent, accumulateMeta(dir, metaFiles))
    }
  }
  
  private def isDirectory(location: File): Boolean = location != null && location.isDirectory()
  private def hasParentDirectory(location: File): Boolean = location != null && isDirectory(location.getParentFile())
  private def isFeatureFile(file: File): Boolean = hasFileExtension("feature", file)
  private def isMetaFile(file: File): Boolean = hasFileExtension("meta", file)
  private def hasFileExtension(extension: String, file: File): Boolean = file.isFile && file.getName().endsWith(s".${extension}")
}

