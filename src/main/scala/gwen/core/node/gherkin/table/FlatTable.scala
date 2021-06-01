/*
 * Copyright 2017-2021 Branko Juric, Brady Wood
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

import gwen.core.state.ScopedData

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
      ((s"${DataTable.recordKey}.index", s"$recordIndex") :: ((s"${DataTable.recordKey}.number", s"${recordIndex + 1}") :: (names.zip(records(recordIndex).zipWithIndex) flatMap { case (name, (value, nameIndex)) =>
          List((s"name[${nameIndex + 1}]", name), (s"data[$name]", value))
        }))).filter(pred)
  }

}
