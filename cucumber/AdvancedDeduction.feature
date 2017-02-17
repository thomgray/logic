@AdvancedDeduction

Feature: Advanced Deductions

  @LEM
  Scenario: LEM
    When I attempt to prove "(AV~A)"
    Then the deduction succeeds
    And show me the deduction

  @LNC
  Scenario: Non contradiction
    When I attempt to prove "~(A & ~A)"
    Then the deduction succeeds
    And show me the deduction

  @ModusTollendoPonens
  Scenario: Modus tollendo ponens
    Given a deduction exists with premises "(AVB)"
    When I attempt to prove "(~A->B)"
    Then the deduction succeeds
    And show me the deduction

  Scenario: Nested DE
    Given a deduction exists with premises "(AVB)"
    When I attempt to prove "((A V C) -> (A V (B & C)))"
    Then the deduction succeeds
    And show me the deduction

  Scenario: Nested DE
    Given a deduction exists with premises "A"
    When I attempt to prove "(B -> (A <-> B))"
    Then the deduction succeeds
    And show me the deduction

  Scenario: Nested DE
    Given a deduction exists with premises "(A V (B V C))"
    When I attempt to prove "( B V (A V C))"
    Then the deduction succeeds
    And show me the deduction

  Scenario: Nested DE
    Given a deduction exists with premises "(AVB)"
    When I attempt to prove "(~A -> B)"
    Then the deduction succeeds
    And show me the deduction

  Scenario:
    When I attempt to prove "((A->B)->(A->(C->B)))"
    Then the deduction succeeds
    And show me the deduction

  Scenario: Complicated DE
    Given a deduction exists with premises "(AVB), (~BVC), (C->~B)"
    When I attempt to prove "A"
    Then the deduction succeeds
    And show me the deduction

    @wip
  Scenario: (P->(QVR)) : ((P->Q)V(P->R))
    Given a deduction exists with premises "(A->(BVC))"
    When I attempt to prove "((A->B)V(A->C))"
    Then the deduction succeeds
    And show me the deduction