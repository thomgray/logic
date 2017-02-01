@Deduction
Feature: Deduction using hard inference

  @CI
  Scenario: Inferring a simple Conjunction Introduction
    Given a deduction exists with premises "A, B"
    When I attempt to prove "(A&B)"
    Then the deduction succeeds
    And the deduction is 3 lines long
    And the 3rd line in the deduction is a CI

  @CP
  Scenario: Inferring Conditional Proof
    Given a deduction exists with premises "A, (A->B), (B->C)"
    When I attempt to prove "(A->C)"
    Then the deduction succeeds

  @MP
  Scenario: Inferring a simple two-step MP
    Given a deduction exists with premises "A, (A->B), (B->C)"
    When I attempt to prove "C"
    Then the deduction succeeds

  @CI
  Scenario: Inferring a simple CI in predicate logic
    Given a deduction exists with premises "Ra, Sf(c)"
    When I attempt to prove "(Ra&Sf(c))"
    Then the deduction succeeds

  @CP
  Scenario: Inferring a multi-step CP
    Given a deduction exists with premises "(B->C), (A->B)"
    When I attempt to prove "(A->C)"
    Then the deduction succeeds
    And show me the deduction

  @DE
  Scenario: Easy DE
    Given a deduction exists with premises "(AVB), (A->B), (C->B)"
    When I attempt to prove "B"
    Then the deduction succeeds
    And show me the deduction

  @DNE
  Scenario: Easy DNE
    Given a deduction exists with premises "~~~~~~A"
    When I attempt to prove "A"
    Then the deduction succeeds
    And the deduction is 4 lines long
    And the 2nd line in the deduction is a DNE
    And the 3rd line in the deduction is a DNE
    And the 4th line in the deduction is a DNE

  @DNI
  Scenario: Easy DNI
    Given a deduction exists with premises "A"
    When I attempt to prove "~~~~A"
    Then the deduction succeeds
    And the deduction is 3 lines long
    And the 2nd line in the deduction is a DNI
    And the 3rd line in the deduction is a DNI

  @DNI
  Scenario: Harder DNI
    Given a deduction exists with premises "(A&B)"
    When I attempt to prove "~~~~A"
    Then the deduction succeeds
    And the deduction is 4 lines long
    And the 2nd line in the deduction is a CE
    And the 3nd line in the deduction is a DNI
    And the 4th line in the deduction is a DNI

  @MT
  Scenario: Easy MT
    Given a deduction exists with premises "(A->B), ~B"
    When I attempt to prove "~A"
    Then the deduction succeeds
    And the deduction is 3 lines long
    And the 3rd line in the deduction is a MT
    And show me the deduction

  @MT @DNI
  Scenario: Harder MT
    Given a deduction exists with premises "(C->(A->B)), ~~~(A->B)"
    When I attempt to prove "~~~C"
    Then the deduction succeeds
    And the deduction is 5 lines long
    And show me the deduction
