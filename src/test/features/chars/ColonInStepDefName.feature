Feature: Colon in StepDef name

  @StepDef
  Scenario: INFO: "<info>"
    Given INFO is "$<info>"

  Scenario: TEST
    Given INFO: "Testing"
