#
# Copyright 2017 Branko Juric, Brady Wood
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

 Feature: Number tables

   Scenario: Single record horizontal data table with no header
       Given single row without header contains a number in decimal and binary form
             | 3 | 11 |

   Scenario: Single record horizontal data table with header
       Given single row with header contains a number in decimal and binary form
             | decimal | binary |
             | 3       | 11     |

   Scenario: Single record vertical data table with no header
       Given single column without header contains a number in decimal and binary form
             | 3  |
             | 11 |

   Scenario: Single record vertical data table with header
       Given single column with header contains a number in decimal and binary form
             | decimal | 3  |
             | binary  | 11 |

   Scenario: Horizontal table with header
       Given each row contains a number and its square and cube
             | number | square | cube |
             | 1      | 1      | 1    |
             | 2      | 4      | 8    |
             | 3      | 9      | 27   |

   Scenario: Horizontal data table with no header
       Given each row contains a number in decimal and binary form
             | 1 | 1  |
             | 2 | 10 |
             | 3 | 11 |

   Scenario: Vertical table with header
       Given each column contains a number and its square and cube
             | number | 1 | 2 | 3  |
             | square | 1 | 4 | 9  |
             | cube   | 1 | 8 | 27 |

   Scenario: Vertical data table with no header
       Given each column contains a number in decimal and binary form
             | 4   | 5    | 6   |
             | 100 | 101  | 110 |

   Scenario: Matrix table with top and left headers
       Given the top and left numbers yield the product in the matrix
             | x | 1 | 2 | 3 |
             | 1 | 1 | 2 | 3 |
             | 2 | 2 | 4 | 6 |
             | 3 | 3 | 6 | 9 |

   Scenario: Horizontal Fibonacci table with no header
       Given each row contains two numbers that sum to a Fibonacci number in the third
             | 0 | 1 | 1 |
             | 1 | 1 | 2 |
             | 1 | 2 | 3 |
             | 2 | 3 | 5 |
             | 3 | 5 | 8 |

   Scenario: tables in nested stepdefs should not conflict
       Given nested tables should match the right tables
