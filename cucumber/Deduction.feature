@Deduction
Feature: Deduction using hard inference

  Scenario: Inferring a simple Conjunction Introduction
    Given a deduction exists with premises "A, B, C"
    When I attempt to prove "(A&B)"
    Then the deduction succeeds

  Scenario: Inferring Conditional Proof
    Given a deduction exists with premises "A, (A->B), (B->C)"
    When I attempt to prove "(A->C)"
    Then the deduction succeeds

  Scenario: Inferring a simple two-step deduction
    Given a deduction exists with premises "A, (A->B), (B->C)"
    When I attempt to prove "C"
    Then the deduction succeeds

  Scenario: Inferring a simple CI in predicate logic
    Given a deduction exists with premises "Ra, Sf(c)"
    When I attempt to prove "(Ra&Sf(c))"
    Then the deduction succeeds

  @wip
  Scenario: Inferring a multi-step CP
    Given a deduction exists with premises "(B->C), (A->B)"
    When I attempt to prove "(A->C)"
    Then the deduction succeeds
    And show me the deduction

  Scenario: Easy DE
    Given a deduction exists with premises "(AVB), (A->B), (C->B)"
    When I attempt to prove "B"
    Then the deduction succeeds
    And show me the deduction