Feature: Binding tests
	    
  Scenario: Check bindings
    Given a1 is "${property.a}"
      And a2 is "${a1}"
     When I bind more properties
     Then a1 should be "A"
      And a2 should be "A"
      And a3 should be "A"
      And b1 should be "B"
      And b2 should be "B"
      And b3 should be "B"
	