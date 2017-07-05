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

/**
  * Enumeration of supported table types.
  *
  * @author Branko Juric
  */
object TableType extends Enumeration {

  val horizontal, vertical, matrix = Value

  def valueFor(headerType: HeaderType.Value): TableType.Value = headerType match {
      case HeaderType.top  => horizontal
      case HeaderType.left => vertical
      case _ => matrix
  }

}

/**
  * Enumeration of supported table header types.
  *
  * @author Branko Juric
  */
object HeaderType extends Enumeration {
  val top, left, top_left  = Value
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
    * Binds each data element to a new table scope for accessing the values.
    */
  def tableScope : ScopedData = new ScopedData("table") {
    override def isEmpty: Boolean = records.isEmpty
    override def findEntries(pred: ((String, String)) => Boolean): Seq[(String, String)] =
      ((names.zipWithIndex map { case (name, index) =>
        (s"name[${index + 1}]", name)
      }) ++ (records.zipWithIndex flatMap { case (record, rIndex) =>
        names.zip(record) flatMap { case (name, value) =>
          (s"data[${rIndex + 1}][$name]", value) :: (if (rIndex == 0) List((s"data[$name]", value)) else Nil)
        }
      })).filter(pred)
  }

  /**
    * Binds each data element in a record to a new for accessing values.
    *
    * @param recordIndex the record index
    */
  def recordScope(recordIndex: Int): ScopedData = new ScopedData("record") {
    override def isEmpty: Boolean = records.isEmpty
    override def findEntries(pred: ((String, String)) => Boolean): Seq[(String, String)] =
      (("record.number", s"${recordIndex + 1}") :: (names.zip(records(recordIndex).zipWithIndex) flatMap { case (name, (value, nameIndex)) =>
          List((s"name[${nameIndex + 1}]", name), (s"data[$name]", value))
        })).filter(pred)
  }

}

/**
  * Models a matrix data table with row and column data names.
  *
  * @param records list of records containing the data
  * @param topNames list of top names
  * @param leftNames list of left names
  * @param vertexName the top-left vertex name
  */
class MatrixTable(val records: List[List[String]], val topNames: List[String], val leftNames: List[String], val vertexName: String) extends DataTable {

  /**
    * Binds each data element to a new table scope for accessing the values.
    */
  def tableScope : ScopedData = new ScopedData("table") {
    override def isEmpty: Boolean = records.isEmpty
    override def findEntries(pred: ((String, String)) => Boolean): Seq[(String, String)] =
      ((("vertex.name", vertexName) :: (topNames.zipWithIndex map { case (topName, topIndex) =>
        (s"top.name[${topIndex + 1}]", topName)
      })) ++ (leftNames.zipWithIndex map { case (leftName, leftIndex) =>
        (s"left.name[${leftIndex + 1}]", leftName)
      }) ++ (records.zipWithIndex flatMap { case (record, leftIndex) =>
        record.zipWithIndex map { case (value, topIndex) =>
          (s"data[${topNames(topIndex)}][${leftNames(leftIndex)}]", value)
        }
      })).filter(pred)
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
        DataTable(step.table.map(_._2), HeaderType.top, namesCSV.split(",").toList)
      case r"""DataTable\(vertical="(.*?)"$namesCSV\)""" =>
        DataTable(step.table.map(_._2), HeaderType.left, namesCSV.split(",").toList)
      case r"""DataTable\(header="(top|left)"$header\)""" =>
        DataTable(step.table.map(_._2), HeaderType.withName(header), Nil)
      case r"""DataTable\(type="matrix"\)""" =>
        DataTable(step.table.map(_._2), HeaderType.top_left, Nil)
      case _ => syntaxError(
        s"""Invalid tag syntax: $tag - correct table tags include: @DataTable(horizontal|vertical="name1,name2..,nameN"), @DataTable(header="top|left"), @DataTable(type="matrix")""")
    }
  }

  /**
    * Creates a data table.
    *
    * @param rawTable the raw data table
    * @param headerType the table  header type
    * @param headers list of header names
    */
  def apply(rawTable: List[List[(String)]], headerType: HeaderType.Value, headers: List[String]): DataTable = {

    val tableType = TableType.valueFor(headerType)

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
        new MatrixTable(records, topHeaders, leftHeaders, table.head(0))
      case _ =>
        if (headers.nonEmpty) new FlatTable(table, headers)
        else new FlatTable(table.tail, table.head)
    }
  }
}


