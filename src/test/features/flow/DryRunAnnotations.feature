Feature: Dry Run Annotations

  @StepDef
  Scenario: I process step one
    Given step is "one"

  @StepDef
  Scenario: I process step two
    Given step is "two"

  @StepDef
  Scenario: I process step three
    Given step is "three"

  @StepDef
  Scenario: I process step four
    Given step4 is "four"

  @StepDef
  @Action
  Scenario: I process step
    Given I process step ${name}

  Scenario: Process multi steps
    Given names is defined by js "['one' ,'two' ,'three'].join(' ')"    @DryRun(name='names',value='one two three')
     When I process step for each name in names delimited by " "
     Then step should be "three"

  Scenario: Process single step
    Given number is defined by js "'four'"
     When I process step ${number}    @DryRun(name='number',value={'one','two','three','four'})
     Then step4 should be "four"
