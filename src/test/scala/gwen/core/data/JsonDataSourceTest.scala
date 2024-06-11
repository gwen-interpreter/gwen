/*
 * Copyright 2024 Branko Juric, Brady Wood
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

package gwen.core.data

import gwen.core.BaseTest

import java.io.File
import org.scalatest.matchers.should.Matchers
import gwen.core.data.DataSource

class JsonDataSourceTest extends BaseTest with Matchers {

    "JSON TopObject" should "return expanded properties and nested arrays" in {

        val (header, records) = getHeaderAndRecords("src/test/resources/data/TopObject.json")
        
        header.size should be (10)
        header(0) should be ("hits")
        header(1) should be ("hits[0].song")
        header(2) should be ("hits[0].year")
        header(3) should be ("hits[1].song")
        header(4) should be ("hits[1].year")
        header(5) should be ("jobs")
        header(6) should be ("jobs[0]")
        header(7) should be ("jobs[1]")
        header(8) should be ("name")
        header(9) should be ("surname")

        records.size should be (1)
        val record = records(0)
        record.size should be (10)
        record(0) should be ("""[{"song":"Just a girl","year":1995},{"song":"What You Waiting For?","year":2004}]""")
        record(1) should be ("Just a girl")
        record(2) should be ("1995")
        record(3) should be ("What You Waiting For?")
        record(4) should be ("2004")
        record(5) should be ("""["Pop Singer","Solo Artist"]""")
        record(6) should be ("Pop Singer")
        record(7) should be ("Solo Artist")
        record(8) should be ("Gwen")
        record(9) should be ("Stefani")

    }

    "JSON NestedObject" should "return expanded properties and nested arrays" in {

        val (header, records) = getHeaderAndRecords("src/test/resources/data/NestedObject.json")
        
        header.size should be (10)
        header(0) should be ("user.hits")
        header(1) should be ("user.hits[0].song")
        header(2) should be ("user.hits[0].year")
        header(3) should be ("user.hits[1].song")
        header(4) should be ("user.hits[1].year")
        header(5) should be ("user.jobs")
        header(6) should be ("user.jobs[0]")
        header(7) should be ("user.jobs[1]")
        header(8) should be ("user.name")
        header(9) should be ("user.surname")

        records.size should be (1)
        val record = records(0)
        record.size should be (10)
        record(0) should be ("""[{"song":"Just a girl","year":1995},{"song":"What You Waiting For?","year":2004}]""")
        record(1) should be ("Just a girl")
        record(2) should be ("1995")
        record(3) should be ("What You Waiting For?")
        record(4) should be ("2004")
        record(5) should be ("""["Pop Singer","Solo Artist"]""")
        record(6) should be ("Pop Singer")
        record(7) should be ("Solo Artist")
        record(8) should be ("Gwen")
        record(9) should be ("Stefani")

    }

    "JSON TopObjectArray" should "return expanded properties and nested arrays for all elements" in {

        val (header, records) = getHeaderAndRecords("src/test/resources/data/TopObjectArray.json")
        
        header.size should be (10)
        header(0) should be ("hits")
        header(1) should be ("hits[0].song")
        header(2) should be ("hits[0].year")
        header(3) should be ("hits[1].song")
        header(4) should be ("hits[1].year")
        header(5) should be ("jobs")
        header(6) should be ("jobs[0]")
        header(7) should be ("jobs[1]")
        header(8) should be ("name")
        header(9) should be ("surname")

        records.size should be (3)
        
        var record = records(0)
        record.size should be (10)
        record(0) should be ("""[{"song":"Just a girl","year":1995},{"song":"What You Waiting For?","year":2004}]""")
        record(1) should be ("Just a girl")
        record(2) should be ("1995")
        record(3) should be ("What You Waiting For?")
        record(4) should be ("2004")
        record(5) should be ("""["Pop Singer","Solo Artist"]""")
        record(6) should be ("Pop Singer")
        record(7) should be ("Solo Artist")
        record(8) should be ("Gwen")
        record(9) should be ("Stefani")

        record = records(1)
        record.size should be (10)
        record(0) should be ("""[{"song":"Extreme Ways","year":2002}]""")
        record(1) should be ("Extreme Ways")
        record(2) should be ("2002")
        record(3) should be ("")
        record(4) should be ("")
        record(5) should be ("""["Singer"]""")
        record(6) should be ("Singer")
        record(7) should be ("")
        record(8) should be ("Moby")
        record(9) should be ("")

        record = records(2)
        record.size should be (10)
        record(0) should be ("")
        record(1) should be ("")
        record(2) should be ("")
        record(3) should be ("")
        record(4) should be ("")
        record(5) should be ("")
        record(6) should be ("")
        record(7) should be ("")
        record(8) should be ("No")
        record(9) should be ("Body")

    }

    "JSON PropertiesArray" should "return expanded properties and nested arrays for all elements" in {

        val (header, records) = getHeaderAndRecords("src/test/resources/data/PropertiesArray.json")
        
        header.size should be (5)
        header(0) should be ("jobs")
        header(1) should be ("jobs[0]")
        header(2) should be ("jobs[1]")
        header(3) should be ("name")
        header(4) should be ("surname")

        records.size should be (3)
        
        var record = records(0)
        record.size should be (5)
        record(0) should be ("Singer")
        record(1) should be ("")
        record(2) should be ("")
        record(3) should be ("Moby")
        record(4) should be ("")

        record = records(1)
        record.size should be (5)
        record(0) should be ("""["Pop Singer","Solo Artist"]""")
        record(1) should be ("Pop Singer")
        record(2) should be ("Solo Artist")
        record(3) should be ("Gwen")
        record(4) should be ("Stefani")

        record = records(2)
        record.size should be (5)
        record(0) should be ("")
        record(1) should be ("")
        record(2) should be ("")
        record(3) should be ("No")
        record(4) should be ("One")

    }

    "JSON NamedStringArray" should "return all string values" in {

        val (header, records) = getHeaderAndRecords("src/test/resources/data/NamedStringArray.json")
        
        header.size should be (1)
        header(0) should be ("name")

        records.size should be (3)
        
        var record = records(0)
        record.size should be (1)
        record(0) should be ("name 1")

        record = records(1)
        record.size should be (1)
        record(0) should be ("name 2")

        record = records(2)
        record.size should be (1)
        record(0) should be ("name 3")

    }

    "JSON NamelessStringArray" should "return all values" in {

        val (header, records) = getHeaderAndRecords("src/test/resources/data/NamelessStringArray.json")
        
        header.size should be (1)
        header(0) should be ("data")

        records.size should be (3)
        
        var record = records(0)
        record.size should be (1)
        record(0) should be ("name 1")

        record = records(1)
        record.size should be (1)
        record(0) should be ("name 2")

        record = records(2)
        record.size should be (1)
        record(0) should be ("name 3")

    }

    "JSON NamelessMixedArray" should "return all values" in {

        val (header, records) = getHeaderAndRecords("src/test/resources/data/NamelessMixedArray.json")
        
        header.size should be (1)
        header(0) should be ("data")

        records.size should be (3)
        
        var record = records(0)
        record.size should be (1)
        record(0) should be ("name 1")

        record = records(1)
        record.size should be (1)
        record(0) should be ("2")

        record = records(2)
        record.size should be (1)
        record(0) should be ("true")

    }

    "JSON EmpyArray" should "return nothing" in {

        val (header, records) = getHeaderAndRecords("src/test/resources/data/EmptyArray.json")
        header.size should be (0)
        records.size should be (0)

    }

    "JSON EmpyObject" should "return nothing" in {

        val (header, records) = getHeaderAndRecords("src/test/resources/data/EmptyObject.json")
        header.size should be (0)
        records.size should be (0)

    }

    "JSON EmpyNamedArray" should "return nothing" in {

        val (header, records) = getHeaderAndRecords("src/test/resources/data/EmptyNamedArray.json")
        header.size should be (0)
        records.size should be (0)

    }

    private def getHeaderAndRecords(filePath: String): (Array[String], Array[Array[String]]) = {
        val ds = DataSource(new File(filePath))
        val table = ds.table
        val header = table.head
        val records = table.tail
        header.size should be (records.transpose.size)
        (header.toArray, records.map(_.toArray).toArray)
    }   
    
}