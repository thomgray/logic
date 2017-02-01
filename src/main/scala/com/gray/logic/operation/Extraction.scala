package com.gray.logic.operation

import com.gray.logic.formula._

trait Extraction {

  def allTerms(domain: Set[Formula]): Set[Term] = {
    def termsForFormula(formula: Formula): Set[Term] = formula match {
      case Equals(left, right) => Set(left, right).flatMap(_.allTerms)
      case Relation(_, args) => args.toSet[Term].flatMap(_.allTerms)
      case Quantifier(variable, formula) => (Set(variable) ++ termsForFormula(formula)).flatMap(_.allTerms)
      case Connective(forms) => forms.toSet.flatMap(termsForFormula).flatMap(_.allTerms)
      case _ => Set.empty[Term]
    }
    domain flatMap termsForFormula
  }

  def allFormulaDecompositions(formulas: Seq[Formula]): Seq[Formula] = {
    (formulas flatMap {
      case f@Biconditional(left, right) => f +: (Seq(Conditional(left, right), Conditional(right, left)) ++ allFormulaDecompositions(Seq(left) ++ allFormulaDecompositions(Seq(right))))
      case f@BinaryConnective(left, right) => f +: (allFormulaDecompositions(Seq(left)) ++ allFormulaDecompositions(Seq(right)))
      case f@Quantifier(_, formula) => f +: allFormulaDecompositions(Seq(formula))
      case f@Negation(formula) => f +: allFormulaDecompositions(Seq(formula))
      case f => Seq(f)
    }).distinct
  }



}
