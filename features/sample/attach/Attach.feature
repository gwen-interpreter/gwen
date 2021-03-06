# 
# Copyright 2021 Brady Wood, Branko Juric
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

   Feature: Attach
	    
  @StepDef
  @Action
  Scenario: I attach my file
       When I attach "README.md" as "nested-readme"
       Then condition should be "false"

  Scenario: Conditional attach
      Given condition is defined by js "true"
       When I attach "README.md" as "readme" if condition
        And I attach my file if condition
       Then condition should be "true"
	