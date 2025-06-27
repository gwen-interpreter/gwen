Feature: Dry Run Annotations - Delimited

  Scenario: Perform all steps
    Given sequence is ""
      And numbers is defined by js "['1', '2', '3'].join(' ')"    @DryRun(name='numbers',value='1 2 3')
     When I perform step ${number} for each number in numbers delimited by " "
     Then numbers should be "1 2 3"
      And sequence should be "112233"
