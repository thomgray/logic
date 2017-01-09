package com.gray.logic.deduction

object InferenceRules extends Enumeration {
  type InferenceRule = Value
  val Premise = Value
  val ConjunctionIntro, ConjunctionElim, DisjunctionIntro, DisjunctionElim = Value
  val ModusPonens, ModusTolles, CP = Value
  val DNE, DNI = Value
  val RAA = Value
  val AssumptionMP, AssumptionRAA, AssumptionDE, AssumptionCP = Value

  val Assumptions = Seq(AssumptionCP, AssumptionMP, AssumptionDE, AssumptionRAA)
}
