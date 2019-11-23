/*
 * Copyright 2015 Branko Juric, Brady Wood
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

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import gwen.errors.JsonPathException
import gwen.eval.{EnvContext, GwenOptions, ScopedDataStack}

class JsonPathSupportTest extends FlatSpec with Matchers {

  val jsonSupport: JsonPathSupport= new EnvContext(GwenOptions())

  val JsonSource = """
  {
    "store": {
      "book": [
        {
          "category": "reference",
          "author": "Nigel Rees",
          "title": "Sayings of the Century",
          "price": 8.95
        },
        {
          "category": "fiction",
          "author": "Evelyn Waugh",
          "title": "Sword of Honour",
          "price": 12.99
        },
        {
          "category": "fiction",
          "author": "Herman Melville",
          "title": "Moby Dick",
          "isbn": "0-553-21311-3",
          "price": 8.99
        },
        {
          "category": "fiction",
          "author": "J. R. R. Tolkien",
          "title": "The Lord of the Rings",
          "isbn": "0-395-19395-8",
          "price": 22.99
        }
      ],
      "bicycle": {
        "color": "red",
        "price": 19.95
      }
    },
    "expensive": 10
  }"""
 
  "match on single attribute" should "return that attribute" in {
    val source = """{
      "id": 10087,
      "name": "my doggie",
      "photoUrls": [],
      "tags": [],
      "status": "available"
    }"""
    jsonSupport.evaluateJsonPath("$.name",source) should be ("my doggie")
  }
  
  "match on authors" should "return all authors" in {
    jsonSupport.evaluateJsonPath("$.store.book[*].author",JsonSource) should be ("""["Nigel Rees","Evelyn Waugh","Herman Melville","J. R. R. Tolkien"]""")
    jsonSupport.evaluateJsonPath("$..author",JsonSource) should be ("""["Nigel Rees","Evelyn Waugh","Herman Melville","J. R. R. Tolkien"]""")
  }
  
  "match on everything in store" should "return all books and bicycles" in {
    jsonSupport.evaluateJsonPath("$.store.*",JsonSource) should be ("""[[{"category":"reference","author":"Nigel Rees","title":"Sayings of the Century","price":8.95},{"category":"fiction","author":"Evelyn Waugh","title":"Sword of Honour","price":12.99},{"category":"fiction","author":"Herman Melville","title":"Moby Dick","isbn":"0-553-21311-3","price":8.99},{"category":"fiction","author":"J. R. R. Tolkien","title":"The Lord of the Rings","isbn":"0-395-19395-8","price":22.99}],{"color":"red","price":19.95}]""")
  }
  
  "match on price of everything" should "return all prices" in {
    jsonSupport.evaluateJsonPath("$.store..price",JsonSource) should be ("""[8.95,12.99,8.99,22.99,19.95]""")
  }
  
  "match on the third book" should "return that book" in {
    jsonSupport.evaluateJsonPath("$..book[2]",JsonSource) should be ("""[{"category":"fiction","author":"Herman Melville","title":"Moby Dick","isbn":"0-553-21311-3","price":8.99}]""")
  } 
  
  "match on first two books" should "return those books" in {
    jsonSupport.evaluateJsonPath("$..book[0,1]",JsonSource) should be ("""[{"category":"reference","author":"Nigel Rees","title":"Sayings of the Century","price":8.95},{"category":"fiction","author":"Evelyn Waugh","title":"Sword of Honour","price":12.99}]""")
  } 
  
  "match on books from index 0 (inclusive) until index 2 (exclusive)" should "return those books" in {
    jsonSupport.evaluateJsonPath("$..book[:2]",JsonSource) should be ("""[{"category":"reference","author":"Nigel Rees","title":"Sayings of the Century","price":8.95},{"category":"fiction","author":"Evelyn Waugh","title":"Sword of Honour","price":12.99}]""")
  } 
  
  "match on books from index 1 (inclusive) until index 2 (exclusive)" should "return those books" in {
    jsonSupport.evaluateJsonPath("$..book[1:2]",JsonSource) should be ("""[{"category":"fiction","author":"Evelyn Waugh","title":"Sword of Honour","price":12.99}]""")
  } 
  
  "match on last two books" should "return those books" in {
    jsonSupport.evaluateJsonPath("$..book[-2:]",JsonSource) should be ("""[{"category":"fiction","author":"Herman Melville","title":"Moby Dick","isbn":"0-553-21311-3","price":8.99},{"category":"fiction","author":"J. R. R. Tolkien","title":"The Lord of the Rings","isbn":"0-395-19395-8","price":22.99}]""")
  } 
  
  "match from end to second last book" should "return those books" in {
    jsonSupport.evaluateJsonPath("$..book[2:]",JsonSource) should be ("""[{"category":"fiction","author":"Herman Melville","title":"Moby Dick","isbn":"0-553-21311-3","price":8.99},{"category":"fiction","author":"J. R. R. Tolkien","title":"The Lord of the Rings","isbn":"0-395-19395-8","price":22.99}]""")
  } 
  
  "match on all books with isbn number" should "return those books" in {
    jsonSupport.evaluateJsonPath("$..book[?(@.isbn)]",JsonSource) should be ("""[{"category":"fiction","author":"Herman Melville","title":"Moby Dick","isbn":"0-553-21311-3","price":8.99},{"category":"fiction","author":"J. R. R. Tolkien","title":"The Lord of the Rings","isbn":"0-395-19395-8","price":22.99}]""")
  } 
  
  "match on all books cheaper than 10 units" should "return those books" in {
    jsonSupport.evaluateJsonPath("$.store.book[?(@.price < 10)]",JsonSource) should be ("""[{"category":"reference","author":"Nigel Rees","title":"Sayings of the Century","price":8.95},{"category":"fiction","author":"Herman Melville","title":"Moby Dick","isbn":"0-553-21311-3","price":8.99}]""")
  } 
  
  "match on non expensive books" should "return those books" in {
    jsonSupport.evaluateJsonPath("$..book[?(@.price <= $['expensive'])]",JsonSource) should be ("""[{"category":"reference","author":"Nigel Rees","title":"Sayings of the Century","price":8.95},{"category":"fiction","author":"Herman Melville","title":"Moby Dick","isbn":"0-553-21311-3","price":8.99}]""")
  } 
  
  "match on books matching regex" should "return those books" in {
    jsonSupport.evaluateJsonPath("$..book[?(@.author =~ /.*REES/i)]",JsonSource) should be ("""[{"category":"reference","author":"Nigel Rees","title":"Sayings of the Century","price":8.95}]""")
  } 
  
  "match on everything" should "return everything" in {
    jsonSupport.evaluateJsonPath("$..*",JsonSource) should be ("""[{"book":[{"category":"reference","author":"Nigel Rees","title":"Sayings of the Century","price":8.95},{"category":"fiction","author":"Evelyn Waugh","title":"Sword of Honour","price":12.99},{"category":"fiction","author":"Herman Melville","title":"Moby Dick","isbn":"0-553-21311-3","price":8.99},{"category":"fiction","author":"J. R. R. Tolkien","title":"The Lord of the Rings","isbn":"0-395-19395-8","price":22.99}],"bicycle":{"color":"red","price":19.95}},10,[{"category":"reference","author":"Nigel Rees","title":"Sayings of the Century","price":8.95},{"category":"fiction","author":"Evelyn Waugh","title":"Sword of Honour","price":12.99},{"category":"fiction","author":"Herman Melville","title":"Moby Dick","isbn":"0-553-21311-3","price":8.99},{"category":"fiction","author":"J. R. R. Tolkien","title":"The Lord of the Rings","isbn":"0-395-19395-8","price":22.99}],{"color":"red","price":19.95},{"category":"reference","author":"Nigel Rees","title":"Sayings of the Century","price":8.95},{"category":"fiction","author":"Evelyn Waugh","title":"Sword of Honour","price":12.99},{"category":"fiction","author":"Herman Melville","title":"Moby Dick","isbn":"0-553-21311-3","price":8.99},{"category":"fiction","author":"J. R. R. Tolkien","title":"The Lord of the Rings","isbn":"0-395-19395-8","price":22.99},"reference","Nigel Rees","Sayings of the Century",8.95,"fiction","Evelyn Waugh","Sword of Honour",12.99,"fiction","Herman Melville","Moby Dick","0-553-21311-3",8.99,"fiction","J. R. R. Tolkien","The Lord of the Rings","0-395-19395-8",22.99,"red",19.95]""")
  } 
  
  "match on number of books" should "return that number" in {
    jsonSupport.evaluateJsonPath("$..book.length()",JsonSource) should be ("""[4]""")
  }
  
  "match on number of books on empty json source" should "error" in {
    intercept[JsonPathException] {
      jsonSupport.evaluateJsonPath("$..book.length()","") should be ("""[4]""")
    }
  }
  
}