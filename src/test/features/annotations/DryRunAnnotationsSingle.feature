Feature: Dry Run Annotations - Single

  Scenario: Perform one step
    Given number is defined by js "'1'"    @DryRun(name='number',value='1')
     When I perform step ${number}
     Then number should be "1"
      And sequence should be "11"
