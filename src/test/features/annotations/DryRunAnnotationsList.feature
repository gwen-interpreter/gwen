Feature: Dry Run Annotations - List

  Scenario: Perform all steps
    Given number is "3"
     When I perform step ${number}    @DryRun(name='number',value=['1','2','3'])
     Then number should be "3"
      And sequence should end with "33"
