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
import gwen.core.Errors._
import gwen.core.node.gherkin.GherkinParser
import gwen.core.node.gherkin.Tag

import org.scalatest.matchers.should.Matchers

/**
  * Data table tests.
  */
class DataTableTest extends BaseTest with Matchers with GherkinParser with ImplicitValueKeys {

  private val parse = parseStep(_: String).get
  
  "Data in multi record horizontal table with header" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable"""), parse(
      """
        |Given a multi record horizontal table with header
        |      | a | b | c |
        |      | 0 | 1 | 1 |
        |      | 1 | 1 | 2 |
        |      | 1 | 2 | 3 |
        |      | 2 | 3 | 5 |
        |      | 3 | 5 | 8 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_multiRecord(dataTable)

  }

  "Data in multi record horizontal table with no header (single quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(horizontal='a,b,c')"""), parse(
      """
        |Given a multi record horizontal table with no header
        |      | 0 | 1 | 1 |
        |      | 1 | 1 | 2 |
        |      | 1 | 2 | 3 |
        |      | 2 | 3 | 5 |
        |      | 3 | 5 | 8 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_multiRecord(dataTable)

  }

  "Data in multi record horizontal table with no header (double quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(horizontal="a,b,c")"""), parse(
      """
        |Given a multi record horizontal table with no header
        |      | 0 | 1 | 1 |
        |      | 1 | 1 | 2 |
        |      | 1 | 2 | 3 |
        |      | 2 | 3 | 5 |
        |      | 3 | 5 | 8 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_multiRecord(dataTable)

  }

  "Data in multi record horizontal table with an impilcit top header" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable"""), parse(
      """
        |Given a multi record horizontal table with a top header
        |      | a | b | c |
        |      | 0 | 1 | 1 |
        |      | 1 | 1 | 2 |
        |      | 1 | 2 | 3 |
        |      | 2 | 3 | 5 |
        |      | 3 | 5 | 8 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_multiRecord(dataTable)

  }

  "Data in multi record horizontal table with a top header (single quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(header='top')"""), parse(
      """
        |Given a multi record horizontal table with a top header
        |      | a | b | c |
        |      | 0 | 1 | 1 |
        |      | 1 | 1 | 2 |
        |      | 1 | 2 | 3 |
        |      | 2 | 3 | 5 |
        |      | 3 | 5 | 8 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_multiRecord(dataTable)

  }

  "Data in multi record horizontal table with a top header (double quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(header="top")"""), parse(
      """
        |Given a multi record horizontal table with a top header
        |      | a | b | c |
        |      | 0 | 1 | 1 |
        |      | 1 | 1 | 2 |
        |      | 1 | 2 | 3 |
        |      | 2 | 3 | 5 |
        |      | 3 | 5 | 8 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_multiRecord(dataTable)

  }

  "Data in multi record vertical table with no header (single quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(vertical='a,b,c')"""), parse(
      """
        |Given a multi record vertical table with no header
        |      | 0 | 1 | 1 | 2 | 3 |
        |      | 1 | 1 | 2 | 3 | 5 |
        |      | 1 | 2 | 3 | 5 | 8 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_multiRecord(dataTable)

  }

  "Data in multi record vertical table with no header (double quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(vertical="a,b,c")"""), parse(
      """
        |Given a multi record vertical table with no header
        |      | 0 | 1 | 1 | 2 | 3 |
        |      | 1 | 1 | 2 | 3 | 5 |
        |      | 1 | 2 | 3 | 5 | 8 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_multiRecord(dataTable)

  }

  "Data in multi record vertical table with a left header (single quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(header='left')"""), parse(
      """
        |Given a multi record horizontal table with a left header
        |      | a | 0 | 1 | 1 | 2 | 3 |
        |      | b | 1 | 1 | 2 | 3 | 5 |
        |      | c | 1 | 2 | 3 | 5 | 8 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_multiRecord(dataTable)

  }

  "Data in multi record vertical table with a left header (double quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(header="left")"""), parse(
      """
        |Given a multi record horizontal table with a left header
        |      | a | 0 | 1 | 1 | 2 | 3 |
        |      | b | 1 | 1 | 2 | 3 | 5 |
        |      | c | 1 | 2 | 3 | 5 | 8 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_multiRecord(dataTable)

  }

  def checkFlatTable_multiRecord(dataTable: FlatTable): Unit = {

    dataTable.records.size should be (5)
    dataTable.records.head.size should be (3)

    val table = dataTable.tableScope
    table.get("name[1]") should be ("a")
    table.get("name[2]") should be ("b")
    table.get("name[3]") should be ("c")
    table.get("data[1][a]") should be ("0")
    table.get("data[1][b]") should be ("1")
    table.get("data[1][c]") should be ("1")
    table.get("data[2][a]") should be ("1")
    table.get("data[2][b]") should be ("1")
    table.get("data[2][c]") should be ("2")
    table.get("data[3][a]") should be ("1")
    table.get("data[3][b]") should be ("2")
    table.get("data[3][c]") should be ("3")
    table.get("data[4][a]") should be ("2")
    table.get("data[4][b]") should be ("3")
    table.get("data[4][c]") should be ("5")
    table.get("data[5][a]") should be ("3")
    table.get("data[5][b]") should be ("5")
    table.get("data[5][c]") should be ("8")
    table.findEntries(_ => true).size should be (18)

    dataTable.records.indices foreach { recIndex =>
      val recNo = recIndex + 1
      val record = dataTable.recordScope(recIndex)
      record.get(`gwen.table.record.index`) should be (s"$recIndex")
      record.get(`gwen.table.record.number`) should be (s"$recNo")
      record.get("name[1]") should be (table.get("name[1]"))
      record.get("name[2]") should be (table.get("name[2]"))
      record.get("name[3]") should be (table.get("name[3]"))
      record.get("data[a]") should be (table.get(s"data[$recNo][a]"))
      record.get("data[b]") should be (table.get(s"data[$recNo][b]"))
      record.get("data[c]") should be (table.get(s"data[$recNo][c]"))
      record.findEntries(_ => true).size should be (8)
    }

  }

  "Data in single record horizontal table with no header (single quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(horizontal='a,b,c')"""), parse(
      """
        |Given a single record horizontal table with no header
        |      | 1 | 2 | 3 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_singleRecord(dataTable)

  }

  "Data in single record horizontal table with no header (double quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(horizontal="a,b,c")"""), parse(
      """
        |Given a single record horizontal table with no header
        |      | 1 | 2 | 3 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_singleRecord(dataTable)

  }

  "Data in single record horizontal table with an implicit top header" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable"""), parse(
      """
        |Given a single record horizontal table with a top header
        |      | a | b | c |
        |      | 1 | 2 | 3 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_singleRecord(dataTable)

  }

  "Data in single record horizontal table with a top header (single quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(header='top')"""), parse(
      """
        |Given a single record horizontal table with a top header
        |      | a | b | c |
        |      | 1 | 2 | 3 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_singleRecord(dataTable)

  }

  "Data in single record horizontal table with a top header (double quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(header="top")"""), parse(
      """
        |Given a single record horizontal table with a top header
        |      | a | b | c |
        |      | 1 | 2 | 3 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_singleRecord(dataTable)

  }

  "Data in single record vertical table with no header (single quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(vertical='a,b,c')"""), parse(
      """
        |Given a single record vertical table with no header
        |      | 1 |
        |      | 2 |
        |      | 3 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_singleRecord(dataTable)

  }

  "Data in single record vertical table with no header (double quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(vertical="a,b,c")"""), parse(
      """
        |Given a single record vertical table with no header
        |      | 1 |
        |      | 2 |
        |      | 3 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_singleRecord(dataTable)

  }

  "Data in single record vertical table with a left header (single quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(header='left')"""), parse(
      """
        |Given a single record vertical table with a left header
        |      | a | 1 |
        |      | b | 2 |
        |      | c | 3 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_singleRecord(dataTable)

  }

  "Data in single record vertical table with a left header (double quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(header="left")"""), parse(
      """
        |Given a single record vertical table with a left header
        |      | a | 1 |
        |      | b | 2 |
        |      | c | 3 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_singleRecord(dataTable)

  }

  def checkFlatTable_singleRecord(dataTable: FlatTable): Unit = {

    dataTable.records.size should be (1)
    dataTable.records.head.size should be (3)

    val table = dataTable.tableScope
    table.get("name[1]") should be ("a")
    table.get("name[2]") should be ("b")
    table.get("name[3]") should be ("c")
    table.get("data[1][a]") should be ("1")
    table.get("data[1][b]") should be ("2")
    table.get("data[1][c]") should be ("3")
    table.findEntries(_ => true).size should be (6)

    dataTable.records.indices foreach { recIndex =>
      val recNo = recIndex + 1
      val record = dataTable.recordScope(recIndex)
      record.get(`gwen.table.record.index`) should be (s"$recIndex")
      record.get(`gwen.table.record.number`) should be (s"$recNo")
      record.get("name[1]") should be (table.get("name[1]"))
      record.get("name[2]") should be (table.get("name[2]"))
      record.get("name[3]") should be (table.get("name[3]"))
      record.get("data[a]") should be (table.get(s"data[$recNo][a]"))
      record.get("data[b]") should be (table.get(s"data[$recNo][b]"))
      record.get("data[c]") should be (table.get(s"data[$recNo][c]"))
      record.findEntries(_ => true).size should be (8)
    }

  }

  "Data in single item horizontal table with no header (single quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(horizontal='a')"""), parse(
      """
        |Given a single item horizontal table with no header
        |      | 1 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_singleItem(dataTable)

  }

  "Data in single item horizontal table with no header (double quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(horizontal="a")"""), parse(
      """
        |Given a single item horizontal table with no header
        |      | 1 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_singleItem(dataTable)

  }

  "Data in single item horizontal table with implicit top header" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable"""), parse(
      """
        |Given a single item horizontal table with a top header
        |      | a |
        |      | 1 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_singleItem(dataTable)

  }

  "Data in single item horizontal table with top header (single quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(header='top')"""), parse(
      """
        |Given a single item horizontal table with a top header
        |      | a |
        |      | 1 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_singleItem(dataTable)

  }

  "Data in single item horizontal table with top header (double quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(header="top")"""), parse(
      """
        |Given a single item horizontal table with a top header
        |      | a |
        |      | 1 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_singleItem(dataTable)

  }

  "Data in single item vertical table with no header (single quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(vertical='a')"""), parse(
      """
        |Given a single item vertical table with no header
        |      | 1 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_singleItem(dataTable)

  }

  "Data in single item vertical table with no header (double quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(vertical="a")"""), parse(
      """
        |Given a single item vertical table with no header
        |      | 1 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_singleItem(dataTable)

  }

  "Data in single item vertical table with left header (single quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(header='left')"""), parse(
      """
        |Given a single item vertical table with a left header
        |      | a | 1 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_singleItem(dataTable)

  }

  "Data in single item vertical table with left header (double quoted)" should "be accessible" in {

    val dataTable = DataTable(
      Tag("""@DataTable(header="left")"""), parse(
      """
        |Given a single item vertical table with a left header
        |      | a | 1 |
      """.stripMargin)).asInstanceOf[FlatTable]

    checkFlatTable_singleItem(dataTable)

  }

  def checkFlatTable_singleItem(dataTable: FlatTable): Unit = {

    dataTable.records.size should be (1)
    dataTable.records.head.size should be (1)

    val table = dataTable.tableScope
    table.get("name[1]") should be ("a")
    table.get("data[1][a]") should be ("1")
    table.findEntries(_ => true).size should be (2)

    dataTable.records.indices foreach { recIndex =>
      val recNo = recIndex + 1
      val record = dataTable.recordScope(recIndex)
      record.get(`gwen.table.record.index`) should be (s"$recIndex")
      record.get(`gwen.table.record.number`) should be (s"$recNo")
      record.get("name[1]") should be (table.get("name[1]"))
      record.get("data[a]") should be (table.get("data[1][a]"))
      record.findEntries(_ => true).size should be (4)
    }

  }

  "Zero item horizontal table with top header (single quoted)" should "return empty table" in {

    DataTable(
      Tag("""@DataTable(header='top')"""), parse(
      """
        |Given a zero item horizontal table with a top header
        |      |  a  |
      """.stripMargin)).asInstanceOf[FlatTable].records.isEmpty should be (true)

  }

  "Zero item horizontal table with top header (double quoted)" should "return empty table" in {

    DataTable(
      Tag("""@DataTable(header="top")"""), parse(
      """
        |Given a zero item horizontal table with a top header
        |      |  a  |
      """.stripMargin)).asInstanceOf[FlatTable].records.isEmpty should be (true)

  }

  "Zero item horizontal table with implicit top header" should "return emtpy table" in {

    DataTable(
      Tag("""@DataTable"""), parse(
      """
        |Given a zero item horizontal table with a top header
        |      |  a  |
      """.stripMargin)).asInstanceOf[FlatTable].records.isEmpty should be (true)

  }

  "Zero item vertical table with left header (single quoted)" should "return empty table" in {

    DataTable(
      Tag("""@DataTable(header='left')"""), parse(
      """
        |Given a zero item vertical table with a left header
        |      |  a  |
      """.stripMargin)).asInstanceOf[FlatTable].records.isEmpty should be (true)

  }

  "Zero item vertical table with left header (double quoted)" should "return empty table" in {

    DataTable(
      Tag("""@DataTable(header="left")"""), parse(
      """
        |Given a zero item vertical table with a left header
        |      |  a  |
      """.stripMargin)).asInstanceOf[FlatTable].records.isEmpty should be (true)

  }

  "Zero table" should "return empty table (single quoted)" in {

    DataTable(Tag("""@DataTable(horizontal='none')"""), parse("Given no table")).asInstanceOf[FlatTable].records.isEmpty should be (true)

  }

  "Zero table" should "return empty table (double quoted)" in {

    DataTable(Tag("""@DataTable(horizontal="none")"""), parse("Given no table")).asInstanceOf[FlatTable].records.isEmpty should be (true)

  }

  "Invalid data table tag syntax" should "error" in {
    checkInvalidTag("@DataTable(horizontal")
    checkInvalidTag("""@DataTable(horizontal="")""")
    checkInvalidTag("""@DataTable(horizontal ="a")""")
    checkInvalidTag("""@DataTable(horizontal= "a")""")
    checkInvalidTag("""@DataTable (horizontal="a" )""")
    checkInvalidTag("""@DataTable (horizontal ="a")""")
    checkInvalidTag("""@DataTable(horizontal="a",unknown="b")""")
    checkInvalidTag("""@DataTable(unknown="a")""")
    checkInvalidTag("""@DataTable(horizontal="a",vertical="b")""")
  }

  def checkInvalidTag(tag: String): Unit = {
    intercept[InvalidTagException] {
      DataTable.checkTagSyntax(Tag(tag))
    }
    intercept[InvalidTagException] {
      DataTable.checkTagSyntax(Tag(tag))
    }
  }

  "Valid data table tags" should "not error" in {
    DataTable.checkTagSyntax(Tag("""@DataTable"""))
    DataTable.checkTagSyntax(Tag("""@DataTable(horizontal='decimal,binary')"""))
    DataTable.checkTagSyntax(Tag("""@DataTable(horizontal="decimal,binary")"""))
    DataTable.checkTagSyntax(Tag("""@DataTable(horizontal='a,b,c')"""))
    DataTable.checkTagSyntax(Tag("""@DataTable(horizontal="a,b,c")"""))
    DataTable.checkTagSyntax(Tag("""@DataTable(header='top')"""))
    DataTable.checkTagSyntax(Tag("""@DataTable(header="top")"""))
    DataTable.checkTagSyntax(Tag("""@DataTable(vertical='a,b,c')"""))
    DataTable.checkTagSyntax(Tag("""@DataTable(vertical="a,b,c")"""))
    DataTable.checkTagSyntax(Tag("""@DataTable(header='left')"""))
    DataTable.checkTagSyntax(Tag("""@DataTable(header="left")"""))
    DataTable.checkTagSyntax(Tag("""@DataTable(horizontal="a")"""))
    DataTable.checkTagSyntax(Tag("""@DataTable(horizontal='a')"""))
    DataTable.checkTagSyntax(Tag("""@DataTable(type='matrix')"""))
    DataTable.checkTagSyntax(Tag("""@DataTable(type="matrix")"""))
  }

}
