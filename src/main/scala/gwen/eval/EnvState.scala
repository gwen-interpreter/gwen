/*
 * Copyright 2014-2020 Branko Juric, Brady Wood
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

import gwen._
import gwen.dsl.BehaviorType

import java.io.File
import java.nio.file.Files
import java.nio.file.StandardCopyOption

class EnvState(val scopes: ScopedDataStack) {

  /** List of current attachments (name-file pairs). */
  private var attachments: List[(String, File)] = Nil
  private var attachmentPrefix = Formatting.padWithZeroes(1)

  /** Stack of behaviors. */
  private var behaviors = List[BehaviorType.Value]()

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
   * Add a file attachment.
   * 
   * @param name the attachment name
   * @param file the file to attach
   */
  def addAttachment(name: String, file: File): (String, File) = { 
    val fileCopy = Files.copy(
      file.toPath, 
      File.createTempFile(s"$attachmentPrefix-${file.simpleName}-", s".${file.extension}").toPath,
      StandardCopyOption.REPLACE_EXISTING
    ).toFile
    fileCopy.deleteOnExit()
    addAttachment((name, fileCopy))
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
      attachmentPrefix = Formatting.padWithZeroes(attachmentPrefix.toInt + 1)
    }

    /** Adds the given behavior to the top of the stack. */
    def addBehavior(behavior: BehaviorType.Value): Unit = {
      behaviors = behavior :: behaviors
    }

    /** Removes the behavior at the top of the stack. */
    def popBehavior(): Option[BehaviorType.Value] = behaviors match {
      case head::tail => 
        behaviors = tail
        Some(head)
      case _ =>
        None
    }

    /** Gets the behavior at the top of the stack. */
    def currentBehavior: Option[BehaviorType.Value] = behaviors.headOption
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