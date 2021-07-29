# 
# Copyright 2016-2020 Brady Wood, Branko Juric
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

   Feature: Binding tests
	    
  Scenario: Check bindings
      Given a1 is "${property.a}"
        And a2 is "${a1}"
       When I bind more properties
       Then a1 should be "A"
        And a2 should be "A"
        And a3 should be "A"
        And b1 should be "B"
        And b2 should be "B"
        And b3 should be "B"
	