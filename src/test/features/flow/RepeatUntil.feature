Feature: Repeat until example

  @StepDef
  @Action
  Scenario: I increment counter
    Given counter is defined by javascript "${counter} + 1"
      And counter > 3 is defined by javascript "${counter} > 3"

  Scenario: Increment counter
    Given counter is "0"
     When @Delay('0s') I increment counter until counter > 3
     Then counter should be "4"

  Scenario: Increment counter with if condition
    Given counter is "-1"
      And condition is defined by javascript "true"
     When I increment counter
      And @Delay('0s') I increment counter until counter > 3 if condition
     Then counter should be "4"
