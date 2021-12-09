# tests gwen-web issue #93
Feature: Test stepdef name containig if keyword

  @Context
  @StepDef
  Scenario: I check if the dashboard is displayed
    Given displayed is "false"

  Scenario: Test stepdef call
    Given I check if the dashboard is displayed
