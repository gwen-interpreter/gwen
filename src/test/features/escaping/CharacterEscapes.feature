Feature: Characters escapes

  @DataTable(header="top")
  @StepDef
  @ForEach
  Scenario: I run a data table test
    Given value is defined by javascript "'${data[value]}'"

  @StepDef
  Scenario: I run a step def test with "<value>" value
    Given value is defined by javascript "'$<value>'"

  Scenario: New line escapes
    Given value1 is defined by javascript "'a\nb'"
      And value2 is 
          """
          ${value1}
          """
      And value3 is defined by javascript 
         """
         (function() {
           
           // escaped new line
           return 'a\nb'

         })();
         """
      And value4 is
          """
          ${value3}
          """
     When I run a step def test with "value with \n character" value
      And I run a data table test
          | value                            |
          | value with \n character          |
          | value with escaped \\n character |
     Then value1 should be
          """
          a
          b
          """
      And value2 should be
          """
          a
          b
          """
      And value3 should be
          """
          a
          b
          """
      And value4 should be
          """
          a
          b
          """

  Scenario: New line double escapes
    Given value1 is defined by javascript "'a\\nb'"
      And value2 is "${value1}"
      And value3 is defined by javascript 
         """
         (function() {
           
           // escaped new line
           return 'a\\nb'

         })();
         """
      And value4 is "${value3}"
     When I run a step def test with "value with \\n character" value
      And I run a data table test
          | value                              |
          | value with \\n character           |
          | value with escaped \\\\n character |
     Then value1 should be "a\nb"
      And value2 should be "a\nb"
      And value3 should be "a\nb"
      And value4 should be "a\nb"

  Scenario: Java escape chars
    Given value is "\t\b\n\r\f\'\"\\"
     Then value should be "\t\b\n\r\f\'\"\\"

  Scenario: Java double escape chars
    Given value is "\\t\\b\\n\\r\\f\\'\\"\\\\"
     Then value should be "\\t\\b\\n\\r\\f\\'\\"\\\\"

