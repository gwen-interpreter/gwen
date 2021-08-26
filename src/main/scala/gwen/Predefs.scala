/*
 * Copyright 2020 Branko Juric, Brady Wood
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

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.matching.Regex

import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.lang3.SystemUtils
import org.apache.commons.text.StringEscapeUtils

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.FileWriter
import java.io.PrintWriter
import java.io.StringWriter
import java.nio.file.{Files, Paths}
import java.text.DecimalFormat
import java.{util => ju}
import java.util.regex.Matcher

/** Predefs and implicits avaiable wherever this page is imported. */

package object gwen {

  val ZeroChar = '‎' // zero width space char
  
  /** Kestrel function for tapping in side effects. */
  implicit class Kestrel[A](val value: A) extends AnyVal { 
    def tap[B](f: A => B): A = { f(value); value }
  }
  
  /** Implicit File IO functions. */
  implicit class FileIO[F <: File](val file: F) extends AnyVal {
    
    def writeText(text: String): File = 
      file tap { f =>
        if (f.getParentFile != null && !f.getParentFile.exists()) {
          f.getParentFile.mkdirs()
        }
        new FileWriter(f) tap { fw =>
          try {
            fw.write(text)
          } finally {
            fw.close()
          }
        }
      }
    
    def writeBinary(bis: BufferedInputStream): File = 
      file tap { f =>
        if (f.getParentFile != null && !f.getParentFile.exists()) {
          f.getParentFile.mkdirs()
        }
        new BufferedOutputStream(new FileOutputStream(f)) tap { bos =>
          try {
            var c = 0
            while ({c = bis.read(); c != -1}) {
              bos.write(c)
            }
          } finally {
            try {
              bis.close()
            } finally {
              bos.close()
            }
          }
        }
      }
    
    def extension: String = file.getName.lastIndexOf(".") match {
      case -1 => ""
      case idx => file.getName.substring(idx + 1)
    }
    
    def writeFile(source: File): Unit = {
      file.writeBinary(new BufferedInputStream(new FileInputStream(source)))
    }

    def readBytes: Array[Byte] = Files.readAllBytes(Paths.get(file.getAbsolutePath))
    
    def deleteDir(): Unit = {
      val files = file.listFiles() 
      if (files != null) { 
        files foreach { _.deleteFile() }
      }
      file.delete()
    }
    
    def deleteFile(): Unit = {
      if (file.isDirectory) {
        file.deleteDir()
      } else {
        file.delete()
      }
    }
    
    def toDir(targetDir: File, targetSubDir: Option[String]): File =
      new File(toPath(targetDir, targetSubDir))
    
    def toPath(targetDir: File, targetSubDir: Option[String]): String =
      targetDir.getPath + File.separator + FileIO.encodeDir(file.getParent) + targetSubDir.map(File.separator + _).getOrElse("")
    
    def toFile(targetDir: File, targetSubDir: Option[String]): File =
      new File(toDir(targetDir, targetSubDir), file.getName)

    def mimeType: String = file.extension match {
      case "png" => "image/png"
      case "json" => "application/json"
      case _ => "text/plain"
    }

    def isSame(other: Option[File]): Boolean = other.exists(_.getCanonicalPath == file.getCanonicalPath)

    def simpleName: String = file.getName.replaceFirst("[.][^.]+$", "")

    def uri: String = FileIO.encodeUri(file.getPath)

  }
  
  object FileIO {
    def encodeDir(dirpath: String): String = 
      if (dirpath != null) dirpath.replaceAll("""[/\:\\]""", "-") else ""
    def encodeUri(path: String): String = {
      if (path != null) {
        if (SystemUtils.IS_OS_WINDOWS) {
          path.replaceAll("\\\\", "/")
        } else {
          path
        }
      } else {
        ""
      }
    }
    def isDirectory(location: File): Boolean = location != null && location.isDirectory
    def hasParentDirectory(location: File): Boolean = location != null && isDirectory(location.getParentFile)
    def isFeatureFile(file: File): Boolean = hasFileExtension("feature", file)
    def isMetaFile(file: File): Boolean = hasFileExtension("meta", file)
    def isCsvFile(file: File): Boolean = hasFileExtension("csv", file)
    def hasFileExtension(extension: String, file: File): Boolean = !file.isDirectory && file.getName.endsWith(s".$extension")
    def recursiveScan(dir: File, extension: String): List[File] = {
      val files = dir.listFiles
      val metas = files.filter(file => hasFileExtension(extension, file)).toList
      files.filter(isDirectory).toList match {
        case Nil => metas
        case dirs => metas ::: dirs.flatMap(dir => recursiveScan(dir, extension))
      }
    }
    def getUserFile(filename: String): Option[File] =
      sys.props.get("user.home").map(new File(_, filename)).filter(_.exists())
    def getFileOpt(filepath: String): Option[File] = Option(new File(filepath)).filter(_.exists())
    def appendFile(files: List[File], file: File): List[File] = appendFile(files, Option(file))
    def appendFile(files: List[File], file: Option[File]): List[File] = (files.filter(!_.isSame(file)) ++ file).distinct
  }
  
  /** Exception functions. */
  implicit class Exceptions[T <: Throwable](val error: T) extends AnyVal {
    
    def writeStackTrace(): String = {
      val sw = new StringWriter()
      val pw = new PrintWriter(sw)
      error.printStackTrace(pw)
      pw.flush()
      pw.close()
      sw.toString
    }
    
  }
  
  /**
    * Implicit regex string interpolator.  This makes it easy to match 
    * incoming steps against regular expressions and capture their parameters.
    */
  implicit class RegexContext(val sc: StringContext) extends AnyVal {
    def r: Regex = {
      new Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*) 
    }
  }
  
  object Formatting {
    
    /**
      * Formats durations for presentation purposes.
      */
    object DurationFormatter {
      
      import scala.concurrent.duration._
  
      private val Formatters = List(
        HOURS -> (("h", new DecimalFormat("00"))),
        MINUTES -> (("m", new DecimalFormat("00"))),
        SECONDS -> (("s", new DecimalFormat("00"))),
        MILLISECONDS -> (("ms", new DecimalFormat("000")))
      )

      /**
        * Formats a given duration to ##h ##m ##s ###ms format.
        * 
        * @param duration the duration to format
        */
      def format(duration: Duration): String = {
        val nanos = duration.toNanos
        val msecs = (nanos / 1000000) + (if ((nanos % 1000000) < 500000) 0 else 1)
        if (msecs > 0) {
          var duration = Duration(msecs, MILLISECONDS)
          Formatters.foldLeft("") { (acc: String, f: (TimeUnit, (String, DecimalFormat))) =>
            val (unit, (unitName, formatter)) = f
            val unitValue = duration.toUnit(unit).toLong
            if (acc.length() == 0 && unitValue == 0) "" 
            else {
              duration = duration - Duration(unitValue, unit)
              s"$acc ${formatter.format(unitValue)}$unitName"
            }
          }.trim.replaceFirst("^0+(?!$)", "")
        } else "~0ms"
      }
  
    }

    def padWithZeroes(num: Int): String = padWithZeroes(num, 4)
    def padWithZeroes(num: Int, padding: Int): String = s"%0${padding}d".format(num)
    def formatDuration(duration: Duration): String = DurationFormatter.format(duration)
    def escapeHtml(text: String): String = StringEscapeUtils.escapeHtml4(text).replaceAll("  ", " &nbsp;").replaceAll("""[\r\n]+""", "<br>")
    def escapeXml(text: String): String = StringEscapeUtils.escapeXml10(text)
    def escapeJson(text: String): String = StringEscapeUtils.escapeJson(text)
    def rightPad(str: String, size: Int): String = if (str.length < size) rightPad(str + " ", size) else str
    def padTailLines(str: String, padding: String) = str.replaceAll("""\r?\n""", s"""\n$padding""")
    def sha256Hash(source: String): String = DigestUtils.sha256Hex(source)

    def resolveParams(source: String, params: List[(String, String)]): (String, List[(String, String)]) = {
      def resolveParams(acc: List[(String, String)], source: String, params: List[(String, String)]): (String, List[(String, String)]) = {
        params match {
          case Nil => (source, acc)
          case head :: tail =>
            val (name, value) = head
            val param = if (source.contains(s"<$name>")) List(head) else Nil
            resolveParams(param ++ acc, source.replaceAll(s"<$name>", Matcher.quoteReplacement(value)), tail)
        }
      }
      resolveParams(Nil, source, params)
    }

    def formatTable(table: List[(Int, List[String])]): String = {
      (table.indices.toList map { rowIndex => formatTableRow(table, rowIndex) }).mkString("\r\n")
    }
    def formatTableRow(table: List[(Int, List[String])], rowIndex: Int): String = {
      val maxWidths = (table map { case (_, rows) => rows.map(_.length) }).transpose.map(_.max)
      s"| ${(table(rowIndex)._2.zipWithIndex map { case (data, dataIndex) => s"${rightPad(data, maxWidths(dataIndex))}" }).mkString(" | ") } |"
    }
    def formatDocString(docString: (Int, String, Option[String]), includeType: Boolean = true) = docString match {
      case (_, content, contentType) =>
        s"""|${"\"\"\""}${if(includeType) contentType.getOrElse("") else ""}
            |$content
            |${"\"\"\""}""".stripMargin
    }
    def formatParams(params: List[(String, String)]): String = {
      if (params.length > 0) {
        s"{ ${params map { case (n, v) => s"$n : $v" } mkString ", "} }"
      } else {
        ""
      }
    }
  }
  
  object DurationOps {
    import scala.concurrent.duration._
    def sum(durations: Seq[Duration]): Duration =
      if (durations.isEmpty) Duration.Zero
      else if (durations.size == 1) durations.head
      else durations.reduce(_+_)
  }

  object StringOps {
    def lastPositionIn(source: String): Position = {
      Source.fromString(s"$source ").getLines().toList match {
        case Nil =>
          Position(1, 1)
        case lines =>
          val lastLength = lines.last.length - 1
          Position(if (lines.size > 0) lines.size else 1, if (lastLength > 0) lastLength else 1)
      }
    }
  }

  object UUIDGenerator {
    val baseId = ju.UUID.randomUUID.toString
    private val counter = new ju.concurrent.atomic.AtomicInteger(0)
    private val lastUuid = ThreadLocal.withInitial[String] { () => baseId }
    def nextId: String = s"$baseId-${counter.incrementAndGet()}" tap { uuid => 
      lastUuid.set(uuid)
    }
    def prevId = lastUuid.get
  }

}
