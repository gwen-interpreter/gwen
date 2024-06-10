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

import com.fasterxml.jackson.databind.ObjectMapper
import com.github.tototoshi.csv.CSVReader
import com.github.tototoshi.csv.defaultCSVFormat
import net.minidev.json.JSONArray

import scala.jdk.CollectionConverters._
import scala.util.Try

import java.io.File
import java.util.AbstractMap
import java.util.ArrayList
import java.util.HashMap

trait DataSource {
  val dataFile: File
  lazy val table: List[List[String]]
  def header: List[String] = table.headOption.getOrElse(Nil)
  def data: List[List[String]] = if (table.size > 1) table.tail else Nil
  def lookupPrefix: String
}

object DataSource {
  val lookupPrefix: String = "data.record."
  def apply(dataFile: File): DataSource = {
    if (FileIO.isCsvFile(dataFile)) {
      new CsvDataSource(dataFile)
    } else if (FileIO.isJsonFile(dataFile)) {
      new JsonDataSource(dataFile)
    } else {
      Errors.unsupportedDataFileError(dataFile)
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
    val trim = GwenSettings.`gwen.auto.trim.data.csv`
    CSVReader.open(dataFile).all().filter(ignoreEmpty).zipWithIndex.map((v, i) => if (i == 0 || trim) v.map(_.trim) else v)
  }
  override def lookupPrefix = CsvDataSource.lookupPrefix
}

object CsvDataSource {
  val lookupPrefix = "csv.record."
}

/** JSON record access */
class JsonDataSource(override val dataFile: File) extends DataSource {
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
          if (list.forall(_.isInstanceOf[String])) {
            List((entry.getKey, JSONArray.toJSONString(listValue)))
          } else {
            list.zipWithIndex map { (v, i) => 
              val name = s"${entry.getKey}.${i}"
              new AbstractMap.SimpleEntry(name, v)
            } flatMap(flatten)
          }
        } getOrElse {
          List((entry.getKey, value.toString))
        }
      }
    } else {
      List((entry.getKey, ""))
    }
  }
  override lazy val table: List[List[String]] = {
    val mapper = new ObjectMapper()
    // name-value pairs
    val json = Try {
      mapper.readValue(dataFile, classOf[ArrayList[HashMap[String, Object]]]).asScala.toList
    } getOrElse {
      List(mapper.readValue(dataFile, classOf[HashMap[String, Object]]))
    }
    val nvps = json.map(_.entrySet().asScala.toList.flatMap(flatten).toMap)
    val header = nvps.map(_.keySet.toList).toList.flatten.distinct.sorted
    val records = nvps match {
      case Nil => Nil
      case recs => 
        recs map { rec => 
          header.map { h => 
            rec.get(h).map(_.toString).getOrElse("")
          }
        }
    }
    val trim = GwenSettings.`gwen.auto.trim.data.json`
    header.map(_.trim) :: records.map(rec => if (trim) rec.map(_.trim) else rec)
  }
  override def lookupPrefix = JsonDataSource.lookupPrefix
}

object JsonDataSource {
  val lookupPrefix = "json.record."
}
