Feature: State codes data feed

  Scenario: Process input state codes
    Given the state code is "${Code}"
      And the state name is "${Name}"
     When I capture the state code as abbreviated state
      And I capture the state name as long state
     Then the state code should be "${abbreviated state}"
      And the state name should be "${long state}"
      And Code should be "ACT" if gwen.data.record.index is "0"
      And Name should be "Australian Capital Territory" if gwen.data.record.number is "1"
