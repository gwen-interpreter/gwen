/*
 * Copyright 2020-2023 Branko Juric, Brady Wood
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

import gwen.core.eval.ComparisonOperator

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.matching.Regex
import scala.util.chaining._
import scala.util.Try

import com.typesafe.scalalogging.LazyLogging
import com.typesafe.scalalogging.Logger
import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.lang3.SystemUtils
import org.apache.commons.text.StringEscapeUtils
import org.htmlcleaner.HtmlCleaner
import org.htmlcleaner.PrettyHtmlSerializer

import java.io.ByteArrayOutputStream
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.FileWriter
import java.io.PrintStream
import java.io.PrintWriter
import java.io.StringReader
import java.io.StringWriter

import java.nio.file.{Files, Paths, StandardCopyOption}
import java.text.DecimalFormat
import java.util.UUID
import java.util.concurrent.Semaphore
import java.util.concurrent.atomic.AtomicInteger
import java.util.Date
import java.text.SimpleDateFormat
import javax.xml.transform.TransformerFactory
import javax.xml.transform.stream.StreamResult
import javax.xml.transform.stream.StreamSource
import javax.xml.transform.OutputKeys

/** Predefs and extension methods avaiable wherever this page is imported. */

/** Extension File IO functions. */
extension [F <: File](file: F) {
  
  def writeText(text: String): File = writeTextToFile(text, append = false)
  def appendText(text: String): File = writeTextToFile(text, append = true)
  private def writeTextToFile(text: String, append: Boolean): File =
    file tap { f =>
      f.newFileWriter(append) tap { fw =>
        try {
          fw.write(text)
        } finally {
          fw.close()
        }
      }
    }

  def writeNewLine(): File = writeNewLineToFile(append = false)
  def appendNewLine(): File = writeNewLineToFile(append = true)
  private def writeNewLineToFile(append: Boolean): File =
    file tap { f =>
      f.newFileWriter(append) tap { fw =>
        try {
          new PrintWriter(fw) tap { pw =>
            try {
              pw.println()
            } finally {
              pw.close()
            }
          }
        } finally {
          fw.close()
        }
      }
    }

  private def newFileWriter(append: Boolean): FileWriter = {
    if (file.getParentFile != null && !file.getParentFile.exists()) {
      file.getParentFile.mkdirs()
    }
    new FileWriter(file, append)
  }

  private def newFileOutputStream: FileOutputStream = {
    if (file.getParentFile != null && !file.getParentFile.exists()) {
      file.getParentFile.mkdirs()
    }
    new FileOutputStream(file)
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

  def dropExtension = file.getName.lastIndexOf(".") match {
    case -1 => file.getName
    case idx => file.getName.substring(0, idx)
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

  def copyToDir(targetDir: File): File = {
    val targetFile = new File(targetDir, file.getName)
    file.copyToFile(targetFile)
  }

  def copyToFile(targetFile: File): File = {
    if (targetFile.getParentFile != null && !targetFile.getParentFile.exists()) {
      targetFile.getParentFile.mkdirs()
    }
    Files.copy(file.toPath, targetFile.toPath, StandardCopyOption.REPLACE_EXISTING).toFile
  }

  def mimeType: String = file.extension match {
    case "png" => "image/png"
    case "json" => "application/json"
    case _ => "text/plain"
  }

  def isSame(other: File): Boolean = isSame(Option(other))
  def isSame(other: Option[File]): Boolean = other.exists(_.getCanonicalPath == file.getCanonicalPath)
  def containsDir(dirName: String ) = {
    Option(new File(file, dirName)).exists(f => f.exists && f.isDirectory)
  }

  def simpleName: String = file.getName.replaceFirst("[.][^.]+$", "")

  def uri: String = FileIO.encodeUri(file.getPath)

}

object FileIO {
  val userDir: Option[File] = sys.props.get("user.home").map(d => new File(d))
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
  def isFeatureOrMetaFile(file: File): Boolean = isFeatureFile(file) || isMetaFile(file)
  def isCsvFile(file: File): Boolean = hasFileExtension("csv", file)
  def isJsonFile(file: File): Boolean = hasFileExtension("json", file)
  def hasFileExtension(extension: String, file: File): Boolean = !file.isDirectory && file.getName.endsWith(s".$extension")
  def recursiveScan(dir: File)(filter: File => Boolean): List[File] = {
    val files = dir.listFiles
    val filtered = files.filter(filter).toList
    files.filter(isDirectory).toList match {
      case Nil => filtered
      case dirs => filtered ::: dirs.flatMap(dir => recursiveScan(dir)(filter))
    }
  }
  def getUserFile(filename: String): Option[File] =
    userDir.map(d => new File(d, filename)).filter(_.exists())
  def getFileOpt(filepath: String): Option[File] = Option(new File(filepath)).filter(_.exists())
  def appendFile(files: List[File], file: File): List[File] = appendFile(files, Option(file))
  def appendFile(files: List[File], file: Option[File]): List[File] = (files.filter(!_.isSame(file)) ++ file).distinct
  def copyClasspathTextResourceToFile(resource: String, targetDir: File, targetFilename: Option[String] = None, allowReplace: Boolean = true): File = {
    new File(targetDir, targetFilename.getOrElse(new File(resource).getName)) tap { targetFile =>
      val exists = targetFile.exists
      if (!allowReplace && exists) Errors.copyResourceError(s"File already exists: $targetFile (use --force option to replace).")
      if (!exists || allowReplace) {
        targetFile.writeText(Source.fromInputStream(getClass.getResourceAsStream(resource)).mkString)
      }
    }
  }
  def copyClasspathBinaryResourceToFile(resource: String, targetDir: File, allowReplace: Boolean = true): File = {
    new File(targetDir, new File(resource).getName) tap { targetFile =>
      val exists = targetFile.exists
      if (!allowReplace && exists) Errors.copyResourceError(s"File already exists: $targetFile (use --force option to replace).")
      if (!exists || allowReplace) {
        targetFile.writeBinary(new BufferedInputStream(getClass.getResourceAsStream(resource)))
      }
    }
  }
}

/** Exception functions. */
extension [T <: Throwable](error: T) {

  def writeStackTrace(): String = {
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    error.printStackTrace(pw)
    pw.flush()
    pw.close()
    sw.toString
  }

  def getMessageLine1: String = {
    Try(Source.fromString(error.getMessage).getLines().next()).getOrElse(error.getMessage)
  }

}

/**
  * Extension regex string interpolator.  This makes it easy to match
  * incoming steps against regular expressions and capture their parameters.
  */
extension (sc: StringContext) {
  def r: Regex = {
    new Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x")*)
  }
}

object Formatting {

  val ZeroChar = '‎' // zero width space char

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
  def escapeHtml(text: String): String = escapeHtmlSpaces(StringEscapeUtils.escapeHtml4(text).replaceAll("""[\r\n]+""", "<br>"))
  def escapeHtmlSpaces(text: String): String = text.replaceAll("  ", " \u00a0")
  def escapeXml(text: String): String = StringEscapeUtils.escapeXml10(text)
  def escapeJson(text: String): String = StringEscapeUtils.escapeJson(text)
  def escapeJava(text: String): String = StringEscapeUtils.escapeJava(text)
  def leftPad(str: String, size: Int): String = s"${" " * (size - str.length)}$str"
  def rightPad(str: String, size: Int): String = s"$str${" " * (size - str.length)}"
  def padTailLines(str: String, padding: String) = str.replaceAll("""\r?\n""", s"""\n$padding""")
  def sha256Hash(source: String): String = DigestUtils.sha256Hex(source)
  def upTo2DecimalPlaces(number: Double): String = new DecimalFormat("#.##").format(number)
  def escapeNewLineChars(source: String): String = source.replaceAll("\n", s"\\\\n");
  def surroundWithQuotes(source: String): String = surroundWithQuotes(source, '\'', '"', '`')
  def surroundWithQuotesForAnnotation(source: String): String = surroundWithQuotes(source, '\'', '`', '"')
  private def surroundWithQuotes(source: String, quote1: Char, quote2: Char, quote3: Char): String = {
    val quoteChar =  
      if (source.contains(quote1)) {
        if (source.contains(quote2)) quote3
        else quote2
      } else quote1
    s"$quoteChar$source$quoteChar"
  } 
  def formatDate(date: String, fromFormat: String, toFormat: String): String = {
    formatDate(new SimpleDateFormat(fromFormat).parse(date), toFormat)
  }
  def formatDate(date: Date, toFormat: String): String = {
    new SimpleDateFormat(toFormat).format(date)
  }

  def formatTable(table: List[(Long, List[String])]): String = {
    (table.indices.toList map { rowIndex => formatTableRow(table, rowIndex) }).mkString("\r\n")
  }
  def formatTableRow(table: List[(Long, List[String])], rowIndex: Int): String = {
    val maxWidths = (table map { case (_, rows) => rows.map(_.length) }).transpose.map(_.max)
    s"| ${(table(rowIndex)._2.zipWithIndex map { case (data, dataIndex) => s"${rightPad(data, maxWidths(dataIndex))}" }).mkString(" | ") } |"
  }
  def formatDocString(docString: (Long, String, Option[String]), includeType: Boolean = true) = docString match {
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

  def splitLines(blob: String): List[String] = blob.split("\\r?\\n").toList

  def prettyPrintXML(xml: String, cDataElements: Option[String]): String = {
    val transformer = TransformerFactory.newInstance().newTransformer()
    transformer.setOutputProperty(OutputKeys.INDENT, "yes")
    cDataElements foreach { cDataElems =>
      transformer.setOutputProperty(OutputKeys.CDATA_SECTION_ELEMENTS, cDataElems)
    }
    transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2")
    val result = new StreamResult(new StringWriter())
    val source = new StreamSource(new StringReader(xml))
    transformer.transform(source, result)
    result.getWriter().toString
  }

  def prettyPrintHTML(html: String): String = {
    val cleaner = new HtmlCleaner()
    val props = cleaner.getProperties
    props.setOmitXmlDeclaration(true)
    val cleanHtml = cleaner.clean(html)
    val pretty = new PrettyHtmlSerializer(props, "  ")
    pretty.getAsString(cleanHtml)
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
  def lastPositionIn(source: String): (Int, Int) = {
    Source.fromString(s"$source ").getLines().toList match {
      case Nil =>
        ((1, 1))
      case lines =>
        val lastLength = lines.last.length - 1
        ((if (lines.size > 0) lines.size else 1, if (lastLength > 0) lastLength else 1))
    }
  }
}

object UUIDGenerator {
  val baseId = UUID.randomUUID.toString
  private val counter = new AtomicInteger(0)
  private val lastUuid = ThreadLocal.withInitial[String] { () => baseId }
  def nextId: String = s"$baseId-${counter.incrementAndGet()}" tap { uuid =>
    lastUuid.set(uuid)
  }
  def prevId = lastUuid.get
}

object Deprecation extends LazyLogging {
  def warn(category: String, oldWay: String, newWay: String): Unit = {
    logger.warn(
      s"""|$category is deprecated and will be unsupported soon
          |${createMsg(category, oldWay, newWay)}""".stripMargin
    )
  }
  def fail(category: String, oldWay: String, newWay: String): Unit = {
    Errors.deprecatedError(
      s"""|$category is deprecated and no longer supported
          |${createMsg(category, oldWay, newWay)}""".stripMargin
    )
  }
  private def createMsg(prefix: String, oldWay: String, newWay: String): String = {
    s"""|       $oldWay >> Instead use >> $newWay
        |       ${"^" * oldWay.size}""".stripMargin
  }
}

object OS {
  def isWindows: Boolean = sys.props.get("os.name").map(_.startsWith("Windows")).getOrElse(false)
}

object Booleans {
  def isTruthy(value: Option[String]): Boolean = !isFalsy(value)
  def isTruthy(value: String): Boolean = !isFalsy(value)
  def isFalsy(value: Option[String]): Boolean = value.map(isFalsy).getOrElse(true)
  def isFalsy(value: String): Boolean = {
    value == null
      || value.isEmpty
      || value.trim == "0"
      || value.trim.toLowerCase == "false"
  }
  def isBoolean(value: String): Boolean = value == "true" || value == "false"
}

object ConsoleColors {
  def isEnabled: Boolean = {
    GwenSettings.`gwen.console.log.colors`
      && Booleans.isFalsy(sys.env.get("CI")) 
      && Booleans.isFalsy(sys.env.get("NO_COLOR"))
  }
}

object Wait {
  /**
    * Waits until a given condition is ready for a given number of seconds.
    * Errors on given timeout out seconds. Checks condition every 1 second.
    *
    * @param timeoutSecs the number of seconds to wait before timing out
    * @param reason a description of what is being waited on
    * @param condition the boolean condition to wait for (until true)
    */
  def waitUntil(timeoutSecs: Long, reason: String)(condition: => Boolean): Unit = {
    val lock = new Semaphore(1)
    lock.acquire()
    val start = System.currentTimeMillis
    while(lock.availablePermits < 1 && ((System.currentTimeMillis - start) / 1000) < timeoutSecs) {
      if (condition) lock.release()
    }
    try {
      if (lock.availablePermits < 1) {
        Errors.waitTimeoutError(timeoutSecs, reason)
      }
    } finally {
      lock.release()
    }
  }
}

enum LocationType:
    case file,url

object Assert {
  def apply(assertion: Boolean, message: => String): Unit = {
    if (!assertion)
      throw new java.lang.AssertionError(message)
  }
  def formatFailed(source: String, expected: String, actual: String, negate: Boolean, operator: ComparisonOperator): String = {
    s"$source should ${if(negate) "not " else ""}$operator ${ValueLiteral.orQuotedValue(expected)}${if (operator == ComparisonOperator.be && actual == expected) "" else s" but ${if (operator == ComparisonOperator.be) "got" else "value was"} ${ValueLiteral.orQuotedValue(actual)}"}"
  }
}
