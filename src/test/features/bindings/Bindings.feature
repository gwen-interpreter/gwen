Feature: Binding tests

  Scenario: Check bindings
    Given a1 is "${property.a}"
      And a2 is "${a1}"
      And j1 is defined by javascript "(function(){ if (true) {return true;} else {return false;} })()"
     When I bind more properties
     Then a1 should be "A"
      And a2 should be "A"
      And a3 should be "A"
      And b1 should be "B"
      And b2 should be "B"
      And b3 should be "B"
      And j1 should be true
