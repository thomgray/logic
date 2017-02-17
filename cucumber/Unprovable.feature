@unprovable
Feature: Unprovable infereces

  Scenario: Unprovable DE
    Given a deduction exists with premises "(AVB)"
    When I attempt to prove "B"
    Then the deduction fails

  Scenario: Unprovable DE with many disjunctions
    Given a deduction exists with premises "(AVB), (BVC), (CVD), (DVE)"
    When I attempt to prove "A"
    Then the deduction fails

  #this totally fails
  Scenario: Unprovable DE with even more disjunctions
    Given a deduction exists with premises "(AVB), (BVC), (CVD), (DVE), (EV(FVG)), (HVI), ((JVK)VL)"
    When I attempt to prove "A"
    Then the deduction fails