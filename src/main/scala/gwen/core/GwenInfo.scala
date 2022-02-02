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

package gwen.core

/** Gwen project information. */
trait GwenInfo {
  lazy val implName: String = Option(this.getClass.getPackage.getImplementationTitle).getOrElse("gwen")
  lazy val implVersion: String = Option(this.getClass.getPackage.getImplementationVersion).getOrElse("-SNAPSHOT")
  lazy val implHome: String = s"https://github.com/gwen-interpreter/$implName"
  lazy val releaseNotesUrl: Option[String] = if (!implVersion.endsWith("-SNAPSHOT")) Some(s"$implHome/releases/tag/v$implVersion") else None
  lazy val gwenHome: String = "https://gweninterpreter.org"
  lazy val noticeMsg: Option[String] = None
}