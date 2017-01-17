package com.gray.logic.formula

case class Conjunction(left: Formula, right: Formula) extends Connective

case class Disjunction(left: Formula, right: Formula) extends Connective

case class Conditional(left: Formula, right: Formula) extends Connective

case class Biconditional(left: Formula, right: Formula) extends Connective

object BinaryConnective{
  def unapply(arg: Connective): Option[(Formula, Formula)] = arg match {
    case Conjunction(l,r) => Some((l,r))
    case Disjunction(l,r) => Some((l,r))
    case Conditional(l,r) => Some((l,r))
    case Biconditional(l,r) => Some((l,r))
    case _ => None
  }
}

case class Negation(formula: Formula) extends Connective


