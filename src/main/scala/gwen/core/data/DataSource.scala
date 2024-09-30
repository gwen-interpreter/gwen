/*
 * Copyright 2023 Branko Juric, Brady Wood
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

package gwen.core.data

import gwen.core.Errors
import gwen.core.FileIO
import gwen.core.GwenSettings
import gwen.core.ImplicitValueKeys

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.exc.MismatchedInputException
import com.github.tototoshi.csv._
import net.minidev.json.JSONArray
import org.apache.commons.lang3.ClassUtils


import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.util.Try

import java.io.File
import java.util.AbstractMap
import java.util.ArrayList
import java.util.HashMap
import com.fasterxml.jackson.core.JsonParseException

trait DataSource {
  val dataFile: File
  lazy val table: List[List[String]]
  def header: List[String] = table.headOption.getOrElse(Nil)
  def data: List[List[String]] = if (table.size > 1) table.tail else Nil
  def lookupPrefix: String
  def parseFile[T](dataFile: File)(parse: File => T): T = {
    try {
      parse(dataFile)
    } catch {
      case e: Throwable => Errors.malformedDataSourceError(dataFile, e)
    }
  }
}

object DataSource extends ImplicitValueKeys {
  val lookupPrefix: String = `data.record.`
  def apply(dataFile: File): DataSource = {
    if (FileIO.isCsvFile(dataFile)) {
      new CsvDataSource(dataFile)
    } else if (FileIO.isJsonFile(dataFile)) {
      new JsonDataSource(dataFile)
    } else {
      Errors.unsupportedFileError(dataFile, "input data", "csv or json")
    }
  }
  def hasLookupPrefix(name: String): Boolean = {
    name.startsWith(lookupPrefix) || name.startsWith(CsvDataSource.lookupPrefix) || name.startsWith(JsonDataSource.lookupPrefix)
  }
}

/** CSV record access */
class CsvDataSource(override val dataFile: File) extends DataSource {
  private def ignoreEmpty(rec: Seq[String]) = rec.filter(_.trim.nonEmpty).nonEmpty
  override lazy val table: List[List[String]] = {
    parseFile(dataFile) { file => 
      val trim = GwenSettings.`gwen.auto.trim.data.csv`
      CSVReader.open(file).all().filter(ignoreEmpty).zipWithIndex.map((v, i) => if (i == 0 || trim) v.map(_.trim) else v)
    }
  }
  override def lookupPrefix = CsvDataSource.lookupPrefix
}

object CsvDataSource {
  val lookupPrefix = "csv.record."
}

/** JSON record access */
class JsonDataSource(override val dataFile: File) extends DataSource {
  override lazy val table: List[List[String]] = {
    parseFile(dataFile) { file => 
      JsonDataSource.parseObject(Source.fromFile(file).mkString)
    }
  }
  override def lookupPrefix = JsonDataSource.lookupPrefix
}

object JsonDataSource {

  val lookupPrefix = "json.record."

  def parseStringArray(jsonStr: String): List[String] = {
    val mapper = new ObjectMapper()
    try {
      mapper.readValue(jsonStr, classOf[java.util.List[String]]).asScala.toList
    } catch {
      case e: MismatchedInputException => 
        Errors.unsupportedJsonStructureError(e)
    }
  }

  def parseObject(jsonStr: String): List[List[String]] = {
    val mapper = new ObjectMapper()
    // name-value pairs
    val json = Try {
      mapper.readValue(jsonStr, classOf[ArrayList[HashMap[String, Object]]]).asScala.toList
    } getOrElse {
      Try {
        List(mapper.readValue(jsonStr, classOf[HashMap[String, Object]]))
      } getOrElse {
        val values = mapper.readValue(jsonStr, classOf[ArrayList[String]])
        val jMap = new HashMap[String, Object]()
        jMap.put("data", values)
        List(jMap)
      }
    }
    val (header, records) = {
      if (json.size == 1 && json.head.entrySet().size == 1 && isStringOrPrimitive(Try(json.head.entrySet().asScala.toList.headOption.map(_.getValue().asInstanceOf[ArrayList[Object]].get(0))).getOrElse(None))) {
        val entry = json.head.entrySet().asScala.toList.head
        (List(entry.getKey()), entry.getValue().asInstanceOf[java.util.List[Object]].asScala.toList.map(v => Option(v).getOrElse("")).map(v => List(v.toString)))
      } else if (isStringOrPrimitive(json.headOption)) {
        (List("data"), json.asInstanceOf[List[Object]].map(v => Option(v).getOrElse("")).map(v => List(v.toString)))
      } else {
        try {
          val nvps = json.map(_.entrySet().asScala.toList.flatMap(flatten).toMap)
          val names = nvps.map(_.keySet.toList).toList.flatten.distinct.sorted
          val values = nvps match {
            case Nil => Nil
            case _ => 
              nvps map { value => 
                names.map { n => 
                  value.get(n).map(_.toString).getOrElse("")
                }
              }
          }
          (names, values)
        } catch {
          case e: ClassCastException =>
            Errors.unsupportedJsonStructureError(e)
        }
      }
    }

    if (header.isEmpty || records.isEmpty || (records.size == 1 && records.head.iterator.next == "[]")) {
      List() :: List()
    } else {
      val trim = GwenSettings.`gwen.auto.trim.data.json`
      header.map(_.trim) :: records.map(rec => if (trim) rec.map(_.trim) else rec)
    }
  }

  private def flatten(entry: java.util.Map.Entry[String, Object]): List[(String, Object)] = {
    val value = entry.getValue
    if (value != null) {
      Try(value.asInstanceOf[java.util.Map[String, Object]]) map { mapValue =>
        mapValue.entrySet().asScala.toList map { e =>
          val name = s"${entry.getKey}.${e.getKey}"
          new AbstractMap.SimpleEntry(name, e.getValue)
        } flatMap(flatten)
      } getOrElse {
        Try(value.asInstanceOf[java.util.List[Object]]) map { listValue => 
          val list = listValue.asScala.toList
          (list.zipWithIndex map { (v, i) => 
            val name = s"${entry.getKey}[${i}]"
            new AbstractMap.SimpleEntry(name, v)
          } flatMap(flatten)) ++ List((entry.getKey, JSONArray.toJSONString(listValue)))
        } getOrElse {
          List((entry.getKey, value.toString))
        }
      }
    } else {
      List((entry.getKey, ""))
    }
  }

  private def isStringOrPrimitive(value: Option[Object]): Boolean = {
      value.map(v => Try(v.asInstanceOf[String]).isSuccess || Try(ClassUtils.isPrimitiveOrWrapper(v.getClass())).getOrElse(false)).getOrElse(false)
  }

}
