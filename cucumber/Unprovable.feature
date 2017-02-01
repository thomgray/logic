@unprovable
Feature: Unprovable infereces

  Scenario: Unprovable DE
    Given a deduction exists with premises "(AVB)"
    When I attempt to prove "B"
    Then the deduction fails