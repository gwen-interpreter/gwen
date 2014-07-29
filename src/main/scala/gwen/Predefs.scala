/*
 * Copyright 2014 Branko Juric, Brady Wood
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

import java.io.FileWriter
import java.io.File
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.FileOutputStream
import java.io.FileInputStream

/**
 * Predefined implicits.
 * 
 * @author Branko Juric
 */
object Predefs {

  /**
   * Kestrel function for tapping in side effects.
   */
  implicit class Kestrel[A](val value: A) extends AnyVal { 
    def tap[B](f: A => B) = { f(value); value } 
  }
  
  /**
   * Implicit File IO functions.
   */
  implicit class FileIO[F <: File](val file: F) extends AnyVal {
    
    def writeText(text: String): File = 
      file tap { f =>
        new FileWriter(f) tap { fw =>
	      try {
	        fw.write(text)
          } finally {
            fw.close
          }
        }
      }
    
    def writeBinary(bis: BufferedInputStream): File = 
      file tap { f =>
        new BufferedOutputStream(new FileOutputStream(f)) tap { bos =>
	      try {
	        var c = 0
	        while ({c = bis.read(); c != -1}) {
	          bos.write(c)
	        }
          } finally {
            try {
              bis.close
            } finally {
              bos.close
            }
          }
        }
      }
    
    def extension: String = file.getName().lastIndexOf(".") match {
      case -1 => ""
      case idx => file.getName().substring(idx + 1)
    }
    
    def writeFile(source: File) {
      file.writeBinary(new BufferedInputStream(new FileInputStream(source)))
    }
    
    def deleteDir() {
      file.listFiles() foreach { f =>
        f.deleteFile
      }
      file.delete()
    }
    
    def deleteFile() {
      if (file.isDirectory()) {
  	    file.deleteDir()
      } else {
        file.delete()
      }
    }
    
  }
  
}