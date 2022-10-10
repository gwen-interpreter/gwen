Feature: Load Strategies

  Background: Init random
    Given random is defined by js "Math.round(Math.round(Math.floor(Math.random() * 1000000)))"

  Scenario: Eager load strategy
    Given random1 is "${random}"
      And @Eager random is defined by js "Math.round(Math.floor(Math.random() * 1000000))"
      And random2 is "${random}"
      And @Eager random is defined by js "Math.round(Math.floor(Math.random() * 1000000))"
      And random3 is "${random}"
      And random4 is "${random}"
      And random 1 equals random 2 is defined by js "${random1} === ${random2}"
      And random 1 equals random 3 is defined by js "${random1} === ${random3}"
      And random 2 equals random 3 is defined by js "${random2} === ${random3}"
      And random 3 equals random 4 is defined by js "${random3} === ${random4}"
     When I capture random 1 equals random 2 as result 1
      And I capture random 1 equals random 3 as result 2
      And I capture random 2 equals random 3 as result 3
      And I capture random 3 equals random 4 as result 4
     Then result 1 should be "false"
      And result 2 should be "false"
      And result 3 should be "false"
      And result 4 should be "true"

  Scenario: Lazy load strategy
    Given random1 is "${random}"
      And @Lazy random is defined by js "Math.round(Math.floor(Math.random() * 1000000))"
      And random2 is "${random}"
      And @Lazy random is defined by js "Math.round(Math.floor(Math.random() * 1000000))"
      And random3 is "${random}"
      And random4 is "${random}"
      And random 1 equals random 2 is defined by js "${random1} === ${random2}"
      And random 1 equals random 3 is defined by js "${random1} === ${random3}"
      And random 2 equals random 3 is defined by js "${random2} === ${random3}"
      And random 3 equals random 4 is defined by js "${random3} === ${random4}"
     When I capture random 1 equals random 2 as result 1
      And I capture random 1 equals random 3 as result 2
      And I capture random 2 equals random 3 as result 3
      And I capture random 3 equals random 4 as result 4
     Then result 1 should be "false"
      And result 2 should be "false"
      And result 3 should be "false"
      And result 4 should be "true"

  Scenario: Default load strategy
    Given random1 is "${random}"
      And random is defined by js "Math.round(Math.floor(Math.random() * 1000000))"
      And random2 is "${random}"
      And random is defined by js "Math.round(Math.floor(Math.random() * 1000000))"
      And random3 is "${random}"
      And random4 is "${random}"
      And random 1 equals random 2 is defined by js "${random1} === ${random2}"
      And random 1 equals random 3 is defined by js "${random1} === ${random3}"
      And random 2 equals random 3 is defined by js "${random2} === ${random3}"
      And random 3 equals random 4 is defined by js "${random3} === ${random4}"
     When I capture random 1 equals random 2 as result 1
      And I capture random 1 equals random 3 as result 2
      And I capture random 2 equals random 3 as result 3
      And I capture random 3 equals random 4 as result 4
     Then result 1 should be "false"
      And result 2 should be "false"
      And result 3 should be "false"
      And result 4 should be "false"

  Scenario: Eager js time
    Given time is "0"
      And @Eager time is defined by js "new Date().getTime()"
      And @Eager time 0 is defined by js "new Date().getTime()"
     When I capture time as time 1
      And I capture time as time 2 
     Then time should not be "0"
      And time 1 should be "${time 2}"
      And time 1 is less than time 0 is defined by js "${time 1} < ${time 0}"
      And time 1 is less than time 0 should be "true"

  Scenario: Lazy js time
    Given time is "0"
      And @Lazy time is defined by js "new Date().getTime()"
      And @Eager time 0 is defined by js "new Date().getTime()"
     When I capture time as time 1
      And I capture time as time 2 
     Then time should not be "0"
      And time 1 should be "${time 2}"
      And time 1 is greater than time 0 is defined by js "${time 1} > ${time 0}"
      And time 1 is greater than time 0 should be "true"

  Scenario: Default js time
    Given time is "0"
      And time is defined by js "new Date().getTime()"
      And @Eager time 0 is defined by js "new Date().getTime()"
     When I capture time as time 1
      And I capture time as time 2 
     Then time should not be "0"
      And time 1 should not be "${time 2}"
      And time 1 is greater than time 0 is defined by js "${time 1} > ${time 0}"
      And time 1 is greater than time 0 should be "true"

  Scenario: Eager, lazy and ephemeral regex years
    Given year 1 is "0"
      And year 2 is "0"
      And year 3 is "0"
      And the date is "2021-09-22"
      And @Eager the eager year is defined in the date by regex "(\d{4})"
      And the date is "2022-09-22"
      And @Lazy the lazy year is defined in the date by regex "(\d{4})"
      And the date is "2023-09-22"
      And the ephemeral year is defined in the date by regex "(\d{4})"
     When I capture the eager year as year 1
      And I capture the lazy year as year 2
      And I capture the ephemeral year as year 3
     Then year 1 should be "2021"
      And year 2 should be "2023"
      And year 3 should be "2023"

  Scenario: Eager, lazy and ephemeral XML statuses
    Given status 1 is "pending"
      And status 2 is "pending"
      And status 3 is "pending"
      And the xml is "<result><status>passed</status></result>"
      And @Eager the eager status is defined by the text in the xml by xpath "//result/status"
      And the xml is "<result><status>failed</status></result>"
      And @Lazy the lazy status is defined by the text in the xml by xpath "//result/status"
      And the xml is "<result><status>sustained</status></result>"
      And the ephemeral status is defined by the text in the xml by xpath "//result/status"
     When I capture the eager status as status 1
      And I capture the lazy status as status 2
      And I capture the ephemeral status as status 3
     Then status 1 should be "passed"
      And status 2 should be "sustained"
      And status 3 should be "sustained"

  Scenario: Eager, lazy and ephemeral JSON statuses
    Given condition is "true"
      And status 1 is "pending"
      And status 2 is "pending"
      And status 3 is "pending"
      And the json is "{ "status": "passed" }"
      And @Eager the eager status is defined in the json by json path "$.status"
      And the json is "{ "status": "failed" }"
      And @Lazy the lazy status is defined in the json by json path "$.status"
      And the json is "{ "status": "sustained" }"
      And the ephemeral status is defined in the json by json path "$.status"
     When I capture the eager status as status 1
      And I capture the lazy status as status 2
      And I capture the ephemeral status as status 3
     Then status 1 should be "passed"
      And status 2 should be "sustained"
      And status 3 should be "sustained"
      And the json should be "{ "status": "sustained" }"  @Message("sustained status expected") if condition
      And the json should be "{ "status": "sustained" }" if condition  @Message("sustained status expected")
      And the json should be "{ "status": "sustained" }"  @Message("sustained status expected")

  Scenario: Eager, lazy and ephemeral system process dates
    Given date 1a is "0"
      And date 1b is "0"
      And date 2a is "0"
      And date 2b is "0"
      And date 3a is "0"
      And date 3b is "0"
      And @Eager the eager date is defined by system process "date"
      And @lazy the lazy date is defined by system process "date"
      And the ephemeral date is defined by system process "date"
     When I capture the eager date as date 1a
      And I wait 1 second
      And I capture the eager date as date 1b
      And I wait 1 second
      And I capture the lazy date as date 2a
      And I wait 1 second
      And I capture the lazy date as date 2b
      And I wait 1 second
      And I capture the ephemeral date as date 3a
      And I wait 1 second
      And I capture the ephemeral date as date 3b
     Then date 1a should not be "0"
      And date 1b should not be "0"
      And date 2a should not be "0"
      And date 2a should not be "0"
      And date 3a should not be "0"
      And date 3a should not be "0"
      And date 1a should be "${date 1b}"
      And date 2a should be "${date 2b}"
      And date 3a should not be "${date 3b}"
      