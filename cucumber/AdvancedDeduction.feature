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

#  (AVB) : ((A V C) -> (A V (B & C)))
#  A : B -> (A <->B)
#  A v (B v C) : B v (A v C)
#  AVB: (~A -> B)


