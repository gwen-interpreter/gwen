/*
 * Copyright 2018 Branko Juric, Brady Wood
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

import gwen.eval.{EnvContext, GwenOptions}

import scala.util.{Failure, Success}

import org.scalatest.{FlatSpec, Matchers}

class TemplateSupportTest extends FlatSpec with Matchers {

  val templateSupport: TemplateSupport = new EnvContext(GwenOptions())

  """Single line static template""" should "match" in {
    val template = """{"id":42,"category":{"name":"pet"},"name":"tiger","status":"available"}"""
    val source = template
    templateSupport.matchTemplate(template, source, "my source").isSuccess should be (true)
  }

  """Single line static template""" should "not match when source is different" in {
    val template = """{"id":42,"category":{"name":"pet"},"name":"tiger","status":"available"}"""
    val source =   """{"id":43,"category":{"name":"pet"},"name":"tiger","status":"available"}"""

    val result = templateSupport.matchTemplate(template, source, "my source")

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

    val result = templateSupport.matchTemplate(template, source, "my source")

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Could not match '{"id":43,"category":{"name":"pet"},"name":"tiger","status":"available"}' at line 1 in my source to '{"id":@{},"category":{"name":"pet"},"name":"tiger","status":"available"}' in template (check literals and/or syntax)""")
    }
  }

  """Single line template""" should "fail when extract tag contains only whitespace" in {
    val template = """{"id":@{   },"category":{"name":"pet"},"name":"tiger","status":"available"}"""
    val source =   """{"id":43,"category":{"name":"pet"},"name":"tiger","status":"available"}"""

    val result = templateSupport.matchTemplate(template, source, "my source")

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Could not match '{"id":43,"category":{"name":"pet"},"name":"tiger","status":"available"}' at line 1 in my source to '{"id":@{   },"category":{"name":"pet"},"name":"tiger","status":"available"}' in template (check literals and/or syntax)""")
    }
  }

  """Single line template""" should "fail when ignore tag has a space" in {
    val template = """{"id":!{ },"category":{"name":"pet"},"name":"tiger","status":"available"}"""
    val source =   """{"id":43,"category":{"name":"pet"},"name":"tiger","status":"available"}"""

    val result = templateSupport.matchTemplate(template, source, "my source")

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Could not match '{"id":43,"category":{"name":"pet"},"name":"tiger","status":"available"}' at line 1 in my source to '{"id":!{ },"category":{"name":"pet"},"name":"tiger","status":"available"}' in template (check literals and/or syntax)""")
    }
  }

  """Single line template""" should "fail when ignore tag is non empty" in {
    val template = """{"id":!{nope},"category":{"name":"pet"},"name":"tiger","status":"available"}"""
    val source =   """{"id":43,"category":{"name":"pet"},"name":"tiger","status":"available"}"""

    val result = templateSupport.matchTemplate(template, source, "my source")

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Could not match '{"id":43,"category":{"name":"pet"},"name":"tiger","status":"available"}' at line 1 in my source to '{"id":!{nope},"category":{"name":"pet"},"name":"tiger","status":"available"}' in template (check literals and/or syntax)""")
    }
  }

  """Single line template""" should "fail when source does not match" in {
    val template = """{"id":!{},"category":{"name":"pet"},"name":"tiger","state":"available"}"""
    val source =   """{"id":42,"category":{"name":"pet"},"name":"tiger","status":"available"}"""

    val result = templateSupport.matchTemplate(template, source, "my source")

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Could not match '{"id":42,"category":{"name":"pet"},"name":"tiger","status":"available"}' at line 1 in my source to '{"id":!{},"category":{"name":"pet"},"name":"tiger","state":"available"}' in template (check literals and/or syntax)""")
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
    templateSupport.matchTemplate(template, source, "my source").get should be (true)
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

    val result = templateSupport.matchTemplate(template, source, "my source")

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

    val result = templateSupport.matchTemplate(template, source, "my source")

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Could not match '  "id": 43,' at line 2 in my source to '  "id": @{},' in template (check literals and/or syntax)""")
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

    val result = templateSupport.matchTemplate(template, source, "my source")

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Could not match '  "id": 43,' at line 2 in my source to '  "id": @{   },' in template (check literals and/or syntax)""")
    }
  }

  """Multi line template""" should "fail when ignore tag has a space" in {
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

    val result = templateSupport.matchTemplate(template, source, "my source")

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Could not match '  "id": 43,' at line 2 in my source to '  "id": !{ },' in template (check literals and/or syntax)""")
    }
  }

  """Multi line template""" should "fail when ignore tag is non empty" in {
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

    val result = templateSupport.matchTemplate(template, source, "my source")

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Could not match '  "id": 43,' at line 2 in my source to '  "id": !{nope},' in template (check literals and/or syntax)""")
    }
  }

  """Multi line template""" should "fail when source does not match" in {
    val template = """{
                     |  "id": !{},
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

    val result = templateSupport.matchTemplate(template, source, "my source")

    result match {
      case Success(_) =>
        fail("Expected match to fail")
      case Failure(err) =>
        err.getMessage should be ("""Expected 'u' but got 'e' at line 7 position 8 in my source: '  "stat[e]..'""")
    }
  }
  
}