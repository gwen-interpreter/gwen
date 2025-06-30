Feature: State codes data feed

  Scenario: Process input state codes in
    Given the state code is "${Code}"
      And the state name is "${Name}"
      And the input csv file is "src/test/resources/data/StateCodes.csv"
      And the input json file is "src/test/resources/data/StateCodes.json"
     When I capture the state code as abbreviated state
      And I capture the state name as long state
     Then the state code should be "${abbreviated state}"
      And the state name should be "${long state}"
      And Code should be "ACT" if gwen.data.record.index is "0"
      And Name should be "Australian Capital Territory" if gwen.data.record.number is "1"
      And Code,Name should be unique in the "${the input csv file}" file
      And Code should be unique in the "${the input csv file}" file
      And Name should be unique in the "${the input csv file}" file @Message("Duplicate Name ${Name}")
      And Code,Name should be unique in the "${the input json file}" file
      And Code should be unique in the "${the input json file}" file
      And Name should be unique in the "${the input json file}" file
      And Code,Name should be unique in the "${the input json file}" file
      And Code,Name should be unique in the input csv file
      And Code should be unique in the input csv file
      And Name should be unique in the input csv file @Message("Duplicate Name ${Name}")
      And Code,Name should be unique in the input json file
      And Code should be unique in the input json file
      And Name should be unique in the input json file
      
