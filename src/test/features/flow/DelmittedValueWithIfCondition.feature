Feature: For each delimited value with if condition

  Scenario: I process options
    Given the type is "my type"
     When I click checbox in "group"
     Then the option should be "option3"
      And the type should be "group"
