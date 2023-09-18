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
import gwen.core.state.ScopedData

/**
  * Data table containing records.
  */
trait DataTable {
  val records: List[List[String]]
  def tableScope: ScopedData
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
        DataTable(if (table.nonEmpty) table.tail else Nil, HeaderType.top, table.headOption.getOrElse(Nil))
      case r"""DataTable\(horizontal=(.+?)$namesCSV\)""" =>
        DataTable(step.table.map(_._2), HeaderType.top, Tag.parseSingleValue(tag.sourceRef, Annotations.DataTable, Some("horizontal"), namesCSV).split(",").toList)
      case r"""DataTable\(vertical=(.+?)$namesCSV\)""" =>
        DataTable(step.table.map(_._2), HeaderType.left, Tag.parseSingleValue(tag.sourceRef, Annotations.DataTable, Some("vertical"), namesCSV).split(",").toList)
      case r"""DataTable\(header=(.+?)$header\)""" if (header.contains("top") || header.contains("left")) =>
        DataTable(step.table.map(_._2), HeaderType.valueOf(Tag.parseSingleValue(tag.sourceRef, Annotations.DataTable, Some("header"), header)), Nil)
      case r"""DataTable\(type=(.+?)$value\)""" if value.contains("matrix") =>
        DataTable(step.table.map(_._2), HeaderType.top_left, Nil)
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
    * @param headerType the table  header type
    * @param headers list of header names
    */
  def apply(rawTable: List[List[(String)]], headerType: HeaderType, headers: List[String]): DataTable = {

    val tableType = TableType.valueFor(headerType)

    if (rawTable.isEmpty && headers.isEmpty)
      Errors.dataTableError(s"Data table expected for StepDef with @DataTable annotation")

    val table = if (tableType == TableType.vertical) rawTable.transpose else rawTable

    if (headers.nonEmpty && table.nonEmpty && headers.size != table.head.size)
      Errors.dataTableError(
        s"""${table.head.size} names expected for data table but ${headers.size} specified: $tableType=\"${headers.mkString(",")}\"""")

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


