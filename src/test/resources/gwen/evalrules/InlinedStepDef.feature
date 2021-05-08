#
# Copyright 2020-2021 Branko Juric, Brady Wood
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

 Feature: Calculator

Scenario: Sum two values
  Given a is "1"
    And b is "2"
   When I add a to b to get c
   Then "c" should be "3"

@StepDef
@When
Scenario: I add a to b to get c
    Given c is defined by javascript "${a} + ${b}"