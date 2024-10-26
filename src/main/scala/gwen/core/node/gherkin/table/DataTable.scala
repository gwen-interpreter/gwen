/*
 * Copyright 2017-2022 Branko Juric, Brady Wood
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
package gwen.core.node.gherkin.table

import gwen.core._
import gwen.core.node.gherkin.Annotations
import gwen.core.node.gherkin.Step
import gwen.core.node.gherkin.Tag
import gwen.core.ImplicitValueKeys
import gwen.core.state.ScopedData

/**
  * Models a data table with a associated data names.
  *
  * @param orientation the table orientation
  * @param records list of records containing the data
  * @param names list of data element names
  */
class DataTable(val orientation: TableOrientation, val records: List[List[String]], val names: List[String]) extends ImplicitValueKeys {

  /**
    * Binds each data element to a new table scope for accessing the values.
    */
  def tableScope : ScopedData = new ScopedData(DataTable.tableKey) {
    override def isEmpty: Boolean = records.isEmpty
    override def findEntries(pred: ((String, String)) => Boolean): Seq[(String, String)] =
      ((names.zipWithIndex map { case (name, index) =>
        (s"name[${index + 1}]", name)
      }) ++ (records.zipWithIndex flatMap { case (record, rIndex) =>
        names.zip(record) map { case (name, value) =>
          (s"data[${rIndex + 1}][$name]", value)
        }
      })).filter(pred)
  }

  /**
    * Binds each data element in a record to a new for accessing values.
    *
    * @param recordIndex the record index
    */
  def recordScope(recordIndex: Int): ScopedData = new ScopedData(DataTable.recordKey) {
    override def isEmpty: Boolean = records.isEmpty
    override def findEntries(pred: ((String, String)) => Boolean): Seq[(String, String)] =
      (
        (`gwen.table.record.index`, s"$recordIndex") :: (
          (`gwen.table.record.number`, s"${recordIndex + 1}") :: (
            names.zip(records(recordIndex).zipWithIndex) map { case (name, (value, nameIndex)) =>
              (s"name[${nameIndex + 1}]", name)
            }
          ) ++ (
            names.zip(records(recordIndex)) map { case (name, value) =>
              (s"data[$name]", value)
            }
          )
        )
      ).filter(pred)
  }

}


/**
  * Data table factory.
  */
object DataTable {

  val tableKey = "table"
  val recordKey = "record"

  def apply(tag: Tag, step: Step): DataTable = {
    tag.name.trim match {
      case r"""DataTable""" =>
        val table = step.table.map(_._2)
        DataTable(if (table.nonEmpty) table.tail else Nil, HeaderPosition.top, table.headOption.getOrElse(Nil))
      case r"""DataTable\(horizontal=(.+?)$namesCSV\)""" =>
        DataTable(step.table.map(_._2), HeaderPosition.top, Tag.parseSingleValue(tag.sourceRef, Annotations.DataTable, Some("horizontal"), namesCSV).split(",").toList)
      case r"""DataTable\(vertical=(.+?)$namesCSV\)""" =>
        DataTable(step.table.map(_._2), HeaderPosition.left, Tag.parseSingleValue(tag.sourceRef, Annotations.DataTable, Some("vertical"), namesCSV).split(",").toList)
      case r"""DataTable\(header=(.+?)$header\)""" if (header.contains("top") || header.contains("left")) =>
        DataTable(step.table.map(_._2), HeaderPosition.valueOf(Tag.parseSingleValue(tag.sourceRef, Annotations.DataTable, Some("header"), header)), Nil)
      case _ => tagSyntaxError(tag)
    }
  }

  private val validTags = List(
    """DataTable""",
    """DataTable\(horizontal='([^".]+?)'\)""",
    """DataTable\(horizontal="([^".]+?)"\)""",
    """DataTable\(vertical='([^''.]+?)'\)""",
    """DataTable\(vertical="([^".]+?)"\)""",
    """DataTable\(header='(top|left)'\)""",
    """DataTable\(header="(top|left)"\)""",
    """DataTable\(type='matrix'\)""",
    """DataTable\(type="matrix"\)"""
    )

  def checkTagSyntax(tag: Tag): Unit =
    if (!validTags.exists(tag.name.matches(_))) tagSyntaxError(tag)

  private def tagSyntaxError(tag: Tag) =
    Errors.invalidTagError(
      s"""Invalid DataTable annotation: $tag - correct syntax is: @DataTable or @DataTable(horizontal|vertical='name1,name2..,nameN') or @DataTable(horizontal|vertical="name1,name2..,nameN") or @DataTable(header='top|left') or @DataTable(header="top|left") or @DataTable(type='matrix') or  @DataTable(type="matrix")"""
    )

  /**
    * Creates a data table.
    *
    * @param rawTable the raw data table
    * @param headerPos the table  header type
    * @param headers list of header names
    */
  def apply(rawTable: List[List[(String)]], headerPos: HeaderPosition, headers: List[String]): DataTable = {

    val orientation = TableOrientation.valueFor(headerPos)

    if (rawTable.isEmpty && headers.isEmpty)
      Errors.dataTableError(s"Data table expected for StepDef with @DataTable annotation")

    val table = if (orientation == TableOrientation.vertical) rawTable.transpose else rawTable

    if (headers.nonEmpty && table.nonEmpty && headers.size != table.head.size)
      Errors.dataTableError(
        s"""${table.head.size} names expected for data table but ${headers.size} specified: $orientation=\"${headers.mkString(",")}\"""")

    if (headers.nonEmpty) new DataTable(orientation, table, headers)
        else new DataTable(orientation, table.tail, table.head)
  }
}


