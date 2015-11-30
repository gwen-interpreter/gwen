#
# Copyright 2014 Branko Juric, Brady Wood
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
 
 Feature: Increment integer
          Uses stepdefs in meta to perform increment operations

Background: Reset
            Resets x to zero
      Given x = 0
            
Scenario: Incrementing 0 should yield 1
    Given x = 0
     When ++x
     Then x == 1
     
Scenario: Addition with stepdef parameters
    Given z = 1 + 2
     Then z == 3
     
Scenario: Addition using stepdef with leading parameter
    Given y = 1
     When z = 2 + y
     Then z == 3
     
Scenario: Addition using stepdef with trailing parameter
    Given x = 1
     When z = x + 2
     Then z == 3


  