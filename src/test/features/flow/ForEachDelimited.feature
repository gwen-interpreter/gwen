Feature: For each delimited value

  Scenario: Process each delimited value
    Given values is "One,Two,Three"
      And x is "${value}" for each value in values delimited by ","
     When I capture values
     Then value should be absent

  @StepDef
  @DataTable
  @Context
  Scenario: I process the following user roles
    Given processed roles is ""
      And processed roles with index is ""
      And processed roles with number is ""
      And I process user roles for each data record
     Then gwen.stepDef.name should be "I process the following user roles"
      And gwen.feature.file.name should be "ForEachDelimited.feature"
      And gwen.feature.file.simpleName should be "ForEachDelimited"
      And gwen.feature.name should be "For each delimited value"
      And gwen.scenario.name should be "Process each delimited value for data record in table"
      And gwen.eval.status.keyword should be "Passed"
      And gwen.eval.status.isPassed should be "true"
      And gwen.eval.status.isFailed should be "false"
      And gwen.eval.status.message should be ""
      And gwen.eval.status.message.escaped should be ""

  @StepDef
  Scenario: I process user roles
    Given processed roles is "${processed roles}${role}" for each role in data[Roles] delimited by ","
      And processed roles with index is "${processed roles with index}${role}[${record.index}][${iteration.index}]" for each role in data[Roles] delimited by ","
      And processed roles with number is "${processed roles with number}${role}[${record.number}][${iteration.number}]" for each role in data[Roles] delimited by ","
      And indexes is "[${record.index}][${iteration.index}]" for each role in data[Roles] delimited by ","
      And numbers is "[${record.number}][${iteration.number}]" for each role in data[Roles] delimited by ","

  Scenario: Process each delimited value for data record in table
    Given I process the following user roles
          | User | Roles       |
          | abc  | role1,role2 |
          | cbd  | role3,role4 |
     When I capture processed roles
     Then processed roles should be "role1role2role3role4"
      And processed roles with index should be "role1[0][0]role2[0][1]role3[1][0]role4[1][1]"
      And processed roles with number should be "role1[1][1]role2[1][2]role3[2][1]role4[2][2]"

  Scenario: For-each on empty iteration should do nothing
    Given items is ""
     When z is "${item}" for each item in items delimited by ","
     Then z should be absent
