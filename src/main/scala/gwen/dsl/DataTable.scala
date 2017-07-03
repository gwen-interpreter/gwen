/*
 * Copyright 2017 Branko Juric, Brady Wood
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
package gwen.dsl

import gwen.errors.{dataTableError, syntaxError}
import gwen.eval.ScopedData
import gwen.Predefs.Kestrel

/**
  * Enumeration of supported table types.
  *
  * @author Branko Juric
  */
object TableType extends Enumeration {
  val horizontal, vertical, matrix = Value
}

/**
  * Data table containing records.
  */
trait DataTable {
  val records: List[List[String]]
  def tableScope: ScopedData
}

/**
  * Models a flat data table with a associated data names.
  *
  * @param records list of records containing the data
  * @param names list of data element names
  */
class FlatTable(val records: List[List[String]], val names: List[String]) extends DataTable {

  /**
    * Binds each data element to a new scope as 'data[recordNo].name' where
    * - recordNo is the record number starting at 1
    * - name is the associated data name
    */
  def tableScope: ScopedData = new ScopedData("table") tap { data =>
    records.zipWithIndex foreach { case (record, rIndex) =>
      names.zip(record) foreach { case (name, value) =>
        if (rIndex == 0) data.set(s"data.$name", value)
        data.set(s"data[${rIndex + 1}].$name", value)
      }
    }
  }

  /**
    * Binds each data element in a record to a new scope as 'data[recordNo][name]' where
    * - recordNo is the record number starting at 1
    * - name is the associated data name
    *
    * @param recordIndex the record index
    */
  def recordScope(recordIndex: Int): ScopedData = new ScopedData("record") tap { data =>
    names.zip(records(recordIndex)) foreach { case (name, value) =>
      data.set(s"data.$name", value)
    }
  }

}

/**
  * Models a matrix data table with row and column data names.
  *
  * @param records list of records containing the data
  * @param names list of top and left header pairs (excluding overlapping top-left)
  * @param topLeft the top-left matrix value
  */
class MatrixTable(val records: List[List[String]], val names: List[(String, String)], val topLeft: String) extends DataTable {

  /**
    * Binds each data element to a new scope as 'data[topName][leftName]' where
    * - topName is the associated top header name
    * - leftName is the associated left header name
    */
  def tableScope: ScopedData = new ScopedData("table") tap { data =>
    data.set("top-left", topLeft)
    val topNames = names.map(_._1)
    val leftNames = names.map(_._2)
    topNames.zipWithIndex foreach { case (topName, topIndex) =>
      data.set(s"top[${topIndex + 1}]", topName)
    }
    leftNames.zipWithIndex foreach { case (leftName, leftIndex) =>
      data.set(s"left[${leftIndex + 1}]", leftName)
    }
    records.zipWithIndex foreach { case (record, leftIndex) =>
      record.zipWithIndex foreach { case (value, topIndex) =>
        data.set(s"data[${topNames(topIndex)}][${leftNames(leftIndex)}]", value)
      }
    }
  }
}

/**
  * Data table factory.
  */
object DataTable {

  import gwen.Predefs.RegexContext

  def apply(tag: Tag, step: Step): DataTable = {
    tag.name.trim match {
      case r"""DataTable\(horizontal="(.*?)"$namesCSV\)""" =>
        DataTable(step.table.map(_._2), TableType.horizontal, namesCSV.split(",").toList)
      case r"""DataTable\(vertical="(.*?)"$namesCSV\)""" =>
        DataTable(step.table.map(_._2), TableType.vertical, namesCSV.split(",").toList)
      case r"""DataTable\(header="(top|left)"$header\)""" =>
        DataTable(step.table.map(_._2), if (header == "top") TableType.horizontal else TableType.vertical, Nil)
      case r"""DataTable\(type="matrix"\)""" =>
        DataTable(step.table.map(_._2), TableType.matrix, Nil)
      case _ => syntaxError(
        s"""Invalid tag syntax: $tag - correct table tags include: DataTable(horizontal|vertical="name1,name2..,nameN"), DataTable(header="top|left"), DataTable(type="matrix")""")
    }
  }

  /**
    * Creates a data table.
    *
    * @param rawTable the raw data table
    * @param tableType the table type (horizontal, vertical, or matrix)
    * @param headers list of header names
    */
  def apply(rawTable: List[List[(String)]], tableType: TableType.Value, headers: List[String]) = {

    if (rawTable.isEmpty)
      dataTableError(s"Data table expected for StepDef with @DataTable annotation")

    val table = if (tableType == TableType.vertical) rawTable.transpose else rawTable

    if (headers.isEmpty && table.size < 2)
      dataTableError(s"Table with header has no records")
    if (headers.nonEmpty && headers.size != table.head.size)
      dataTableError(s"${table.head.size} data names expected for $tableType data table but got ${headers.size}: '${headers}'")

    tableType match {
      case TableType.matrix =>
        val topHeaders = table.head.tail
        val leftHeaders = table.transpose.head.tail
        val records = table.tail.map(_.tail)
        new MatrixTable(records, topHeaders zip leftHeaders, table.head(0))
      case _ =>
        if (headers.nonEmpty) new FlatTable(table, headers)
        else new FlatTable(table.tail, table.head)
    }
  }
}


