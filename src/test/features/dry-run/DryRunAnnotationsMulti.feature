Feature: Dry Run Annotations (Multi)

  @StepDef
  @Action
  Scenario: I perform step 1
     Then number should be "1"

  @StepDef
  @Action
  Scenario: I perform step 2
     Then number should not be "1"

  Scenario: Perform all steps (multiple values)
    Given numbers is defined by js "['1', '2'].join(' ')"
     When I perform step ${number} for each number in numbers delimited by " "    @DryRun(name='number',value={'1','2'})
     Then numbers should be "1 2"

  Scenario: Perform all steps (delimited values)
    Given numbers is defined by js "['1', '2'].join(' ')"    @DryRun(name='numbers',value='1 2')
     When I perform step ${number} for each number in numbers delimited by " "
     Then numbers should be "1 2"
