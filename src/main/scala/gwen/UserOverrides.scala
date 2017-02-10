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

package gwen

import java.io.File

/**
  * Applies user overrides to properties and meta files before they are loaded 
  * into the interpreter. The overrides are only applied when either or both the 
  * gwen.properties and gwen.meta files exist in the user's home directory.
  */
object UserOverrides {

  val UserProperties: Option[File] = getUserFile("gwen.properties")
  val UserMeta: Option[File] = getUserFile("gwen.meta")

  /**
    * Adds the gwen.properties user override file (if it exists) to the end of the given 
    * list of properties files and removes any duplicates.
    * 
    * @param properties the list of properties files to add the user override 
    *                   properties file to
    * @return the list of properties files (including the user override properties file)
    */
  def addUserProperties(properties: List[File]): List[File] = addUserFile(properties, UserProperties)
  
  /**
    * Adds the gwen.meta user override file (if it exists) to the end of the given list 
    * of meta files and removes any duplicates.
    * 
    * @param metaFiles the list of meta files to add the override meta file to
    * @return the list of properties files (including the user override meta file)
    */
  def addUserMeta(metaFiles: List[File]): List[File] = addUserFile(metaFiles, UserMeta)
  
  /**
    * Merges two lists of meta files together, making sure that there are no 
    * duplicates and that the gwen.meta user override file (if it exists) is 
    * appended to the end of the merged list.
    * 
    * @return the merged meta file list
    */
  def mergeMetaFiles(metaFiles: List[File], metaOverrides: List[File]): List[File] =
    addUserMeta(metaFiles ++ metaOverrides)
  
  private def addUserFile(files: List[File], userFile: Option[File]) =
    (files.filter(!isSameFile(_, userFile)) ++ userFile).distinct
    
  private def isSameFile(file1: File, file2: Option[File]): Boolean = 
    file2.exists(_.getCanonicalPath().equals(file1.getCanonicalPath()))
    
  private def getUserFile(filename: String): Option[File] = 
    sys.props.get("user.home").map(new File(_, filename)).filter(_.exists())
    
}