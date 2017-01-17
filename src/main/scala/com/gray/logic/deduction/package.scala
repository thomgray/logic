package com.gray.logic

package object deduction {


  object InferenceRule extends Enumeration {

    val MP, MT, CE, CI, DI, DE, DNE, DNI, CP, RAA, BE, BI = Value

    val Premise, AssCP, AssRAA, AssDE, Reiteration = Value

    def string(rule: InferenceRule.Value) = rule match {
      case MP => "Modus Ponens"
      case MT => "Modus Tollens"
      case CE => "Conjunction Elimination"
      case CI => "Conjunction Introduction"
      case DE => "Disjunction Elimination"
      case DI => "Disjunction Introduction"
      case DNE => "Double-Negation Elimination"
      case DNI => "Double-Negation Introduction"
      case CP => "Conditional Proof"
      case RAA =>"Reductio Ad Absurdum"
      case BE =>"Biconditional Elimination"
      case BI =>"Biconditional Introduction"

      case AssCP => "Assumption (CP)"
      case AssRAA => "Assumption (RAA)"
      case AssDE => "Assumption (DE)"

      case other => other.toString
    }

    val assumptions = Seq(AssCP, AssDE, AssRAA)
  }


}
