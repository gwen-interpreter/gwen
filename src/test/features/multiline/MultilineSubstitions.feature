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
      And a should not be "${gwen.eval.duration}"
      And b should not be "${gwen.eval.duration.msecs}"
      And b should not be "${gwen.eval.duration.secs}"
