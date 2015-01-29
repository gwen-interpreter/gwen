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

package gwen.eval

import java.io.File

/**
 * Captures a feature file and its associated meta as a unit. If a gwen.meta
 * file exists in the user home directory, it is implicitly added to the
 * end of the list of meta to provide user override capability.
 * 
 * @param featureFile
 * 				the feature file
 * @param metas
 * 				the associated meta files (if any)
 */
class FeatureUnit(val featureFile: File, private val metas: List[File]) {
  val metaFiles = (metas.filter(!_.getCanonicalPath().equals(FeatureUnit.UserMeta.map(_.getCanonicalPath()).getOrElse(""))) ++ FeatureUnit.UserMeta).distinct
}

/**
 * Feature unit factory.
 */
object FeatureUnit {
  val UserMeta = sys.props.get("user.home").map(new File(_, "gwen.meta")).filter(_.exists())
}