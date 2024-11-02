/*
 * Copyright 2018-2021 Branko Juric, Brady Wood
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

import gwen.core.BaseTest
import gwen.core.state.StateLevel
import gwen.core.state.TopScope

import scala.util.{Failure, Success}

import org.scalatest.matchers.should.Matchers

class TemplateSupportTest extends BaseTest with Matchers with TemplateSupport {

  val topScope = new TopScope(StateLevel.feature)

  """Single line static template""" should "match" in {
    val template = """{"id":42,"category":{"name":"pet"},"name":"tiger","status":"available"}"""
    val source = template
    matchTemplate(template, source, "my source", topScope).isSuccess should be (true)
  }

  """Single line static template""" should "not match when source is different" in {
    val template = """{"id":42,"category":{"name":"pet"},"name":"tiger","status":"available"}"""
    val source =   """{"id":43,"category":{"name":"pet"},"name":"tiger","status":"available"}"""

    val result = matchTemplate(template, source, "my source", topScope)

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Expected '2' but got '3' at line 1 position 8 in my source: '{"id":4[3]..'""")
    }
  }

  """Single line template""" should "fail when extract tag is empty" in {
    val template = """{"id":@{},"category":{"name":"pet"},"name":"tiger","status":"available"}"""
    val source =   """{"id":43,"category":{"name":"pet"},"name":"tiger","status":"available"}"""

    val result = matchTemplate(template, source, "my source", topScope)

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Failed to match '{"id":43,"category":{"name":"pet"},"name":"tiger","status":"available"}' at line 1 in my source to '{"id":@{},"category":{"name":"pet"},"name":"tiger","status":"available"}' in template""")
    }
  }

  """Single line template""" should "fail when extract tag contains only whitespace" in {
    val template = """{"id":@{   },"category":{"name":"pet"},"name":"tiger","status":"available"}"""
    val source =   """{"id":43,"category":{"name":"pet"},"name":"tiger","status":"available"}"""

    val result = matchTemplate(template, source, "my source", topScope)

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Failed to match '{"id":43,"category":{"name":"pet"},"name":"tiger","status":"available"}' at line 1 in my source to '{"id":@{   },"category":{"name":"pet"},"name":"tiger","status":"available"}' in template""")
    }
  }

  """Single line template""" should "fail when placholder prefix is unknown" in {
    val template = """{"id":!{pet.id},"category":{"name":"pet"},"name":"tiger","status":"available"}"""
    val source =   """{"id":43,"category":{"name":"pet"},"name":"tiger","status":"available"}"""

    val result = matchTemplate(template, source, "my source", topScope)

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Expected '!' but got '4' at line 1 position 7 in my source: '{"id":[4]..'""")
    }
  }

  """Single line template""" should "fail when placholder prefix is unknown and has a space" in {
    val template = """{"id":!{ },"category":{"name":"pet"},"name":"tiger","status":"available"}"""
    val source =   """{"id":43,"category":{"name":"pet"},"name":"tiger","status":"available"}"""

    val result = matchTemplate(template, source, "my source", topScope)

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Expected '!' but got '4' at line 1 position 7 in my source: '{"id":[4]..'""")
    }
  }

  """Single line template""" should "fail when plaeholder is unkown" in {
    val template = """{"id":!{nope},"category":{"name":"pet"},"name":"tiger","status":"available"}"""
    val source =   """{"id":43,"category":{"name":"pet"},"name":"tiger","status":"available"}"""

    val result = matchTemplate(template, source, "my source", topScope)

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Expected '!' but got '4' at line 1 position 7 in my source: '{"id":[4]..'""")
    }
  }

  """Single line template""" should "fail when source does not match" in {
    val template = """{"id":@{*},"category":{"name":"pet"},"name":"tiger","state":"available"}"""
    val source =   """{"id":42,"category":{"name":"pet"},"name":"tiger","status":"available"}"""

    val result = matchTemplate(template, source, "my source", topScope)

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Failed to match '{"id":42,"category":{"name":"pet"},"name":"tiger","status":"available"}' at line 1 in my source to '{"id":@{*},"category":{"name":"pet"},"name":"tiger","state":"available"}' in template""")
    }
  }

  """Multi line static template""" should "match" in {
    val template = """{
                     |  "id": 42,
                     |  "category": {
                     |    "name": "pet"
                     |  },
                     |  "name": "tiger",
                     |  "status": "available"
                     |}""".stripMargin
    val source = template
    matchTemplate(template, source, "my source", topScope).get should be (true)
  }

  """Multi line static template""" should "not match when source is different" in {
    val template = """{
                     |  "id": 42,
                     |  "category": {
                     |    "name": "pet"
                     |  },
                     |  "name": "tiger",
                     |  "status": "available"
                     |}""".stripMargin
    val source  = """{
                     |  "id": 43,
                     |  "category": {
                     |    "name": "pet"
                     |  },
                     |  "name": "tiger",
                     |  "status": "available"
                     |}""".stripMargin

    val result = matchTemplate(template, source, "my source", topScope)

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Expected '2' but got '3' at line 2 position 10 in my source: '  "id": 4[3]..'""")
    }
  }

  """Multi line template""" should "fail when extract tag is empty" in {
    val template = """{
                     |  "id": @{},
                     |  "category": {
                     |    "name": "pet"
                     |  },
                     |  "name": "tiger",
                     |  "status": "available"
                     |}""".stripMargin
    val source  = """{
                     |  "id": 43,
                     |  "category": {
                     |    "name": "pet"
                     |  },
                     |  "name": "tiger",
                     |  "status": "available"
                     |}""".stripMargin

    val result = matchTemplate(template, source, "my source", topScope)

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Failed to match '  "id": 43,' at line 2 in my source to '  "id": @{},' in template""")
    }
  }

  """Multi line template""" should "fail when extract tag contains only whitespace" in {
    val template = """{
                     |  "id": @{   },
                     |  "category": {
                     |    "name": "pet"
                     |  },
                     |  "name": "tiger",
                     |  "status": "available"
                     |}""".stripMargin
    val source  = """{
                     |  "id": 43,
                     |  "category": {
                     |    "name": "pet"
                     |  },
                     |  "name": "tiger",
                     |  "status": "available"
                     |}""".stripMargin

    val result = matchTemplate(template, source, "my source", topScope)

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Failed to match '  "id": 43,' at line 2 in my source to '  "id": @{   },' in template""")
    }
  }

  """Multi line template""" should "fail when placeholder is unkown" in {
    val template = """{
                     |  "id": !{ },
                     |  "category": {
                     |    "name": "pet"
                     |  },
                     |  "name": "tiger",
                     |  "status": "available"
                     |}""".stripMargin
    val source  = """{
                     |  "id": 43,
                     |  "category": {
                     |    "name": "pet"
                     |  },
                     |  "name": "tiger",
                     |  "status": "available"
                     |}""".stripMargin

    val result = matchTemplate(template, source, "my source", topScope)

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Expected '!' but got '4' at line 2 position 9 in my source: '  "id": [4]..'""")
    }
  }

  """Multi line template""" should "fail when ignore placheolder is unknown" in {
    val template = """{
                     |  "id": !{nope},
                     |  "category": {
                     |    "name": "pet"
                     |  },
                     |  "name": "tiger",
                     |  "status": "available"
                     |}""".stripMargin
    val source  = """{
                     |  "id": 43,
                     |  "category": {
                     |    "name": "pet"
                     |  },
                     |  "name": "tiger",
                     |  "status": "available"
                     |}""".stripMargin

    val result = matchTemplate(template, source, "my source", topScope)

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Expected '!' but got '4' at line 2 position 9 in my source: '  "id": [4]..'""")
    }
  }

  """Multi line template""" should "fail when source does not match" in {
    val template = """{
                     |  "id": @{*},
                     |  "category": {
                     |    "name": "pet"
                     |  },
                     |  "name": "tiger",
                     |  "status": "available"
                     |}""".stripMargin
    val source  = """{
                     |  "id": 42,
                     |  "category": {
                     |    "name": "pet"
                     |  },
                     |  "name": "tiger",
                     |  "state": "available"
                     |}""".stripMargin

    val result = matchTemplate(template, source, "my source", topScope)

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Expected 'u' but got 'e' at line 7 position 8 in my source: '  "stat[e]..'""")
    }
  }
  
}