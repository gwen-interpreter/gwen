Feature: User ${user.name} ${user.surname}

  Scenario: capture user job
    Given job one is "${user.jobs[0]}"
      And job two is "${user.jobs[1]}"
      And job count is defined by js "${user.jobs}.length"
     When I capture job one as job 1
      And I capture job two as job 2
      And I capture job count as count
     Then user.name should not be blank
      And user.surname should not be blank
      And job 1 should not be blank
      And job 1 should not be "${job 2}"
      And count should be "2"