package com.gray.logic.formula

abstract class Compositor extends Formula

abstract class Connective extends Compositor

object Connective {
  def unapply(arg: Connective): Option[(Seq[Formula])] = arg match {
    case Negation(formula) => Some(Seq(formula))
    case Conjunction(f1, f2) => Some(Seq(f1,f2))
    case Disjunction(f1, f2) => Some(Seq(f1,f2))
    case Conditional(f1, f2) => Some(Seq(f1,f2))
    case Biconditional(f1, f2) => Some(Seq(f1,f2))
  }
}

abstract class Quantifier(val variable: Variable, val formula: Formula) extends Compositor

object Quantifier {
  def unapply(arg: Quantifier): Option[(Variable, Formula)] = Some((arg.variable, arg.formula))
}
