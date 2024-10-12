Feature: Multiline substitutions

  Scenario: Multiline parameter and property substition
    Given a is
          """
          multiline
          value
          """
      And b is "${a}"
     When c is assigned to "${a}"
      And d is assigned to
          """
          ${a}
          """
     Then b should be "${a}"
      And b should be
          """
          ${a}
          """
      And c should be "${a}"
      And c should be
          """
          ${a}
          """
      And d should be "${a}"
      And d should be
          """
          ${a}
          """
      And a should not be "${gwen.feature.eval.duration}"
      And b should not be "${gwen.feature.eval.duration.msecs}"
      And b should not be "${gwen.feature.eval.duration.secs}"
      And a should not be "${gwen.scenario.eval.duration}"
      And b should not be "${gwen.scenario.eval.duration.msecs}"
      And b should not be "${gwen.scenario.eval.duration.secs}"
