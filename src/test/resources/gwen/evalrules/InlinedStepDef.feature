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
