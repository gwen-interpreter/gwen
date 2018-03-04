#
# Copyright 2018 Branko Juric, Brady Wood
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

  Feature: Match and extract JSON templates


 Scenario: Init
     Given my pet status is "available"


 Scenario: Match static single line JSON template
     Given my value is
           """
           {"id":42,"category":{"name":"pet"},"name":"tiger","status":"available"}
           """
      Then my value should match template "{"id":42,"category":{"name":"pet"},"name":"tiger","status":"available"}"
       And my value should match template
           """
           {"id":42,"category":{"name":"pet"},"name":"tiger","status":"available"}
           """
       And my value should match template file "features/sample/templates/json/StaticSingleLineTemplate.json"


 Scenario: Match static multi line JSON template
     Given my value is
           """
           {
             "id": 42,
             "category": {
               "name": "pet"
             },
             "name": "tiger",
             "status": "available"
           }
           """
      Then my value should not match template "{"id":42,"category":{"name":"pet"},"name":"tiger","status":"available"}"
       And my value should match template
           """
           {
             "id": 42,
             "category": {
               "name": "pet"
             },
             "name": "tiger",
             "status": "available"
           }
           """
       And my value should match template file "features/sample/templates/json/StaticMultiLineTemplate.json"


 Scenario: Match dynamic single line JSON template (1 ignore, 1 extract, 1 inject)
     Given my value is
           """
           {"id":42,"category":{"name":"pet"},"name":"tiger","status":"available"}
           """
      Then my value should match template "{"id":@{},"category":{"name":"pet"},"name":"@{pet name}","status":"${my pet status}"}"
       And category name should be absent
       And pet id should be absent
       And pet name should be "tiger"


 Scenario: Match dynamic multi line JSON template (2 extracts, 1 ignore, 1 inject)
     Given my value is
           """
           {
             "id": 42,
             "category": {
               "name": "pet"
             },
             "name": "tiger",
             "status": "available"
           }
           """
      Then my value should match template
           """
           {
             "id": @{pet id},
             "category": {
               "name": "@{category name}"
             },
             "name": "@{}",
             "status": "${my pet status}"
           }
           """
       And pet id should be "42"
       And category name should be "pet"
       And the pet name is defined in my value by json path "$.name"
       And the pet name should be "tiger"
       And the pet status is defined in my value by json path "$.status"
       And the pet status should be "available"


 Scenario: Match dynamic single line JSON template file (1 ignore, 1 extract, 1 inject)
     Given my value is
           """
           {"id":42,"category":{"name":"pet"},"name":"tiger","status":"available"}
           """
      Then my value should match template file "features/sample/templates/json/DynamicSingleLineTemplate.json"
       And category name 1 should be absent
       And pet id 1 should be absent
       And pet name 1 should be "tiger"

  Scenario: Match dynamic multi line JSON template file (2 extracts, 1 ignore, 1 inject)
     Given my value is
           """
           {
             "id": 42,
             "category": {
               "name": "pet"
             },
             "name": "tiger",
             "status": "available"
           }
           """
      Then my value should match template file "features/sample/templates/json/DynamicMultiLineTemplate.json"
       And pet id 2 should be "42"
       And category name 2 should be "pet"
       And the pet name is defined in my value by json path "$.name"
       And the pet name should be "tiger"
       And the pet status is defined in my value by json path "$.status"
       And the pet status should be "available"