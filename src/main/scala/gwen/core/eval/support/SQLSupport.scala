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

package gwen.core.eval.support

import gwen.core._
import gwen.core.state.SensitiveData

import java.sql.DriverManager
import java.sql.Connection

object SQLSupport {

  private def settingKey(database: String, key: String) = s"gwen.db.$database.$key"
  private def getDriver(database: String) = Settings.get(settingKey(database, "driver"))
  private def getUrl(database: String) = Settings.get(settingKey(database, "url"))

   /** Checks that DB settings are present. */
  def checkDBSettings(database: String): Unit = {
    SQLSupport.getDriver(database)
    SQLSupport.getUrl(database)
  }

}

/** Can be mixed into evaluation engines to provide SQL support. */
trait SQLSupport {

  /**
    * Evaluates an SQL query against a database and returns the first column value of the first row in the result.
    * 
    * @param sql the SQL statement to execute
    * @param database the database name
    * @return the first column value of the first row in the result
    */
  def executeSQLQuery(sql: String, database: String): String = {
    if (sql.trim().length() == 0) {
      Errors.sqlError("Cannot evaluate empty SQL statement")
    }
    if (database.trim().length() == 0) {
      Errors.sqlError("Database not specified, please provide name of database setting to use: gwen.db.<name>")
    }

    try {
      withDatabase(database) { connection =>
        val stmt = connection.createStatement()
        try {
          val result = stmt.executeQuery(sql)
          if (result.next) {
            result.getString(1)
          }
          else Errors.sqlError(s"SQL did not return a result: $sql")
        } finally {
          stmt.close()
        }
      }
    } catch {
      case e: Exception => Errors.sqlError(s"Failed to run SQL query: ${sql}, reason is: ${e}")
    }
  }

  /**
    * Executes an SQL update statement against a database and returns the number of rows affected.
    *
    * @param sql the SQL update statement to execute
    * @param database the database name
    * @return the number of rows affected (as a string)
    */
  def executeSQLUpdate(sql: String, database: String): Int = {
    if (sql.trim().length() == 0) {
      Errors.sqlError("Cannot execute empty SQL statement")
    }
    if (database.trim().length() == 0) {
      Errors.sqlError("Database not specified, please provide name of database setting to use: gwen.db.<name>")
    }

    try {
      withDatabase(database) { connection =>
        val stmt = connection.createStatement()
        try {
          stmt.executeUpdate(sql)
        } finally {
          stmt.close()
        }
      }
    } catch {
      case e: Exception => Errors.sqlError(s"Failed to run SQL update statement: ${sql}, reason is: ${e}")
    }
  }

  private def withDatabase[T](database: String)(apply: Connection => T): T = {
    SensitiveData.withValue(SQLSupport.getDriver(database)) { driver =>
      SensitiveData.withValue(SQLSupport.getUrl(database)) { url =>
        Class.forName(driver)
        val connection = DriverManager.getConnection(url)
        try {
          apply(connection)
        } finally {
          connection.close()
        }
      }
    }
  }

}
