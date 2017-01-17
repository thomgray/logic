package com.gray.logic.formula

abstract class Term {
  def allTerms: Set[Term] = this match {
    case c: Constant => Set[Term](c)
    case v: Variable => Set[Term](v)
    case f: Function => Set[Term](f) ++ f.args.toSet[Term].flatMap(_.allTerms)
  }
}

case class Variable(index: Int) extends Term

case class Function(index: Int, args: Seq[Term]) extends Term

case class FunctionScheme(index: Int) {
  def apply(terms: Term*) = Function(index, terms)
}

case class Constant(index: Int) extends Term