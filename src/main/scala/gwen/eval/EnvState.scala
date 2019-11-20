/*
 * Copyright 2014-2019 Branko Juric, Brady Wood
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

import gwen.Predefs.FileIO
import gwen.Predefs.Formatting._
import gwen.Predefs.Kestrel
import java.io.File
import gwen.dsl.Scenario
import gwen.dsl.Step

class EnvState(val scopes: ScopedDataStack) {

  /** List of current attachments (name-file pairs). */
  private var attachments: List[(String, File)] = Nil
  private var attachmentPrefix = padWithZeroes(1)

  /** Map of for-each StepDefs. */
  private var foreachStepDefs: Map[String, Scenario] = Map[String, Scenario]()

  /** Checks if attachments exist. */
  def hasAttachments: Boolean = attachments.nonEmpty

  /** Returns current attachments before clearing them. */
  def popAttachments(): List[(String, File)] = attachments tap { _ =>
    attachments = Nil
  }

  /**
   * Add an attachment.
   * 
   * @param name the attachment name
   * @param extension the filename extension
   * @param content the content to write to the file
   */
  def addAttachment(name: String, extension: String, content: String): (String, File) = { 
    val file = File.createTempFile(s"$attachmentPrefix-", s".$extension")
    file.deleteOnExit()
    Option(content) foreach { file.writeText }
    addAttachment((name, file))
  }

  /**
    * Adds an attachment
    * 
    * @param attachment the attachment (name-file pair) to add
    */
    private def addAttachment(attachment: (String, File)): (String, File) = 
    (attachment match {
      case (name, file) => 
        if (file.getName.startsWith(s"$attachmentPrefix-")) attachment
        else {
          val prefixedFilename = s"$attachmentPrefix-${file.getName}"
          val prefixedFile = if (file.getParentFile != null) new File(file.getParentFile, prefixedFilename) else new File(prefixedFilename)
          if (file.renameTo(prefixedFile)) (name, prefixedFile)
          else attachment
        }
    }) tap { att =>
      attachments = att :: attachments
      attachmentPrefix = padWithZeroes(attachmentPrefix.toInt + 1)
    }

    /** Adds for-each StepDef for a given step. */
    def addForeachStepDef(step: Step, stepDef: Scenario): Unit = {
      foreachStepDefs += (step.uniqueId -> stepDef)
    }

    /** Gets the optional for-each StepDef for a given step. */
    def popForeachStepDef(step: Step): Option[Scenario] = 
      foreachStepDefs.get(step.uniqueId) tap { stepDef =>
        if (stepDef.nonEmpty) {
          foreachStepDefs -= step.uniqueId 
        }
      }
}

object EnvState {
  def apply(topScope: TopScope): EnvState = {
    new EnvState(new ScopedDataStack()) tap { newState => 
      topScope.implicitAtts foreach { case (n, v) => 
        newState.scopes.topScope.set(n, v)
      }
    }
  }
}