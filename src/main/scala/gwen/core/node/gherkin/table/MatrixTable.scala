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
  * Models a matrix data table with row and column data names.
  *
  * @param records list of records containing the data
  * @param topNames list of top names
  * @param leftNames list of left names
  * @param vertexName the top-left vertex name
  */
class MatrixTable(val records: List[List[String]], val topNames: List[String], val leftNames: List[String], val vertexName: String) extends DataTable {

  val tableType: TableType = TableType.matrix

  /**
    * Binds each data element to a new table scope for accessing the values.
    */
  def tableScope : ScopedData = new ScopedData(DataTable.tableKey) {
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
