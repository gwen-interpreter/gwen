/*
 * Copyright 2015-2021 Branko Juric, Brady Wood
 * Copyright 2018 Alexandru Cuciureanu
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

package gwen.core.engine.support

import gwen.core.Settings

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

import org.scalatest._
import slick.jdbc
import slick.jdbc.H2Profile.api._
import slick.jdbc.JdbcBackend
import slick.jdbc.JdbcBackend.Database
import slick.lifted.Tag

class SQLSupportTest
    extends FlatSpec
    with Matchers
    with BeforeAndAfterAll
    with SQLSupport {

  private implicit val dbConfig: DBConfig = DBConfig(driver = "org.h2.Driver", dbName = "testdb")

  private implicit val db: JdbcBackend.DatabaseDef = Database.forURL(
    url = dbConfig.dbUrl,
    driver = dbConfig.driver,
    executor = AsyncExecutor(name = "",
                             minThreads = 1,
                             maxThreads = 1,
                             queueSize = 1,
                             maxConnections = 1)
  )

  override def beforeAll(): Unit = {
    Settings.set("gwen.db.testdb.driver", dbConfig.driver)
    Settings.set("gwen.db.testdb.url", dbConfig.dbUrl)

    Await.result(awaitable = DB().seed, atMost = 5 seconds)
  }

  override def afterAll(): Unit = {
    db.close()
  }

  "executeSQLQuery" should "return the first column value of the first row in the result" in {
    val statement = "SELECT * FROM CAKES"
    executeSQLQuery(statement, dbConfig.dbName) shouldBe "Brownie"
  }

  "executeSQLUpdate" should "update statement against a database and returns the number of rows affected" in {
    val statement =
      """
        |UPDATE CAKES
        |SET CAKE_NAME = 'Awesome Butterkuchen'
        |WHERE PRICE = 3
      """.stripMargin

    executeSQLUpdate(statement, dbConfig.dbName) shouldBe 1
  }
}

private class DBConfig(val driver: String, val dbName: String) {

  val dbUrl: String =
    s"jdbc:h2:mem:$dbName;DB_CLOSE_DELAY=-1;DATABASE_TO_UPPER=false"
}

private object DBConfig {
  def apply(driver: String, dbName: String): DBConfig =
    new DBConfig(driver, dbName)
}

private object DB {
  def apply()(implicit db: jdbc.JdbcBackend.DatabaseDef,
              dBConfig: DBConfig): DB = new DB(db)
}

private class DB(db: JdbcBackend.DatabaseDef)(implicit dBConfig: DBConfig) {
  Class.forName(dBConfig.driver)

  private val cakes = TableQuery[Cakes]

  def seed: Future[Unit] = {
    db.run(setup)
  }

  private def setup = DBIO.seq(
    cakes.schema.create,
    cakes += (("Brownie", 2)),
    cakes += (("Butterkuchen", 3)),
    cakes += (("Apple Cake", 5))
  )

}

private class Cakes(tag: Tag) extends Table[(String, Double)](tag, "CAKES") {
  def * = (name, price)

  def name = column[String]("CAKE_NAME", O.PrimaryKey)

  def price = column[Double]("PRICE")
}
