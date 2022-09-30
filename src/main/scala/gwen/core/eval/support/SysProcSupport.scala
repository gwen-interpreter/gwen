/*
 * Copyright 2022 Branko Juric, Brady Wood
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

package gwen.core.eval.support

import gwen.core.Errors
import gwen.core.state.SensitiveData

import scala.sys.process.stringToProcess
import scala.sys.process.stringSeqToProcess
import scala.util.Failure
import scala.util.Success
import scala.util.Try

trait SysProcSupport {

  def callSysProc(sysproc: String, delimiter: Option[String], unix: Boolean): String = {
    Try {
      SensitiveData.withValue(sysproc) { sproc =>
        delimiter match {
          case None => 
              if (unix) Seq("/bin/sh", "-c", sproc).!! else sproc.!!
          case Some(delim) => 
            SensitiveData.withValue(delim) { d =>
              if (unix) (Seq("/bin/sh", "-c") ++ sproc.split(d).toSeq).!! else sproc.split(d).toSeq.!!
            }
        }
      }
    } match {
      case Success(output) => Option(output).map(_.trim).getOrElse("")
      case Failure(e) => Errors.systemProcessError(s"The call to system process '$sysproc' failed", e)
    }
  }

}
