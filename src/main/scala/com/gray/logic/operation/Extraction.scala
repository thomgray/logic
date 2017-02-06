package com.gray.logic.operation

import com.gray.logic.formula._

trait Extraction {

  def allTerms(domain: Set[Formula]): Set[Term] = {
    def termsForFormula(formula: Formula): Set[Term] = formula match {
      case Equals(left, right) => Set(left, right).flatMap(_.allTerms)
      case Relation(_, args) => args.toSet[Term].flatMap(_.allTerms)
      case Quantifier(variable, f) => (Set(variable) ++ termsForFormula(f)).flatMap(_.allTerms)
      case Connective(forms) => forms.toSet.flatMap(termsForFormula).flatMap(_.allTerms)
      case _ => Set.empty[Term]
    }
    domain flatMap termsForFormula
  }

  def allFormulaDecompositions(formulas: Seq[Formula]): Seq[Formula] = formulas.flatMap(_.decompositions).flatMap{
    case f@Biconditional(left, right) => Seq(f, Conditional(left, right), Conditional(right, left))
    case other => Seq(other)
  }.distinct


}
