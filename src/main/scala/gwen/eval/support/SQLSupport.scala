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

package gwen.eval.support

import gwen.errors._
import java.sql.DriverManager

import gwen.Settings
import gwen.eval.EnvContext

/** Can be mixed into evaluation engines to provide SQL support. */
trait SQLSupport {
  this: EnvContext =>

  /**
    * Evaluates an SQL query against a database and returns the result.
    * 
    * @param sql the SQL statement to execute
    * @param database the database name
    * @return the result of executing the SQL
    */
  def evaluateSql(sql: String, database: String): String =
    evaluate("$[dryRun:sql]") {
      if (sql.trim().length() == 0) {
        sqlError("Cannot evaluate empty SQL statement")
      }
      if (database.trim().length() == 0) {
        sqlError("Database not specified, please provide name of database setting to use: gwen.db.<name>")
      }

      try {
        Class.forName(Settings.get(s"gwen.db.${database}.driver"))
        val connection = DriverManager.getConnection(Settings.get(s"gwen.db.${database}.url"))
        try {
          val stmt = connection.createStatement()
          try {
            val result = connection.createStatement().executeQuery(sql)
            if (result.next) {
              result.getString(1)
            }
            else sqlError(s"SQL did not return a result: $sql")
          } finally {
            stmt.close()
          }
        } finally {
          connection.close()
        }
      } catch {
        case e: Exception => sqlError(s"Failed to evaluate SQL statement: ${sql}, reason is: ${e}")
      }
    }
                        
}