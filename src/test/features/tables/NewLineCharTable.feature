Feature: New line character in tables

  @DataTable(header="top")
  @StepDef
  @ForEach
  Scenario: Data table test
    Given value is defined by javascript "'${data[value]}'"

  @StepDef
  Scenario: I run normal scenario test with "<value>" value
    Given value is defined by javascript "'$<value>'"

  Scenario: Test
   When I run normal scenario test with "value with \n character" value
   Then Data table test
        | value                            |
        | value with \n character          |
        | value with escaped \\n character |
