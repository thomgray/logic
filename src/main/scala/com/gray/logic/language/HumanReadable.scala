package com.gray.logic.language

import com.gray.logic.formula._

trait HumanReadable {

  val domain: Domain = new Domain

  class ConditionalFactory {
    def apply(left: Formula, right: Formula) = Conditional(left, right)
  }

  class BiconditionalFactory {
    def apply(left: Formula, right: Formula)= Biconditional(left, right)
  }

  class ConjunctionFactory {
    def apply(left: Formula, right: Formula) = Conjunction(left, right)
  }

  class DisjunctionFactory {
    def apply(left: Formula, right: Formula) = Disjunction(left, right)
  }

  class NegationFactory {
    def apply(formula: Formula) = formula.negation()
    def unapply(arg: Negation): Option[Formula] = Some(arg.formula)
  }

  class ReallyNegationFactory {
    def apply(formula: Formula) = Negation(formula)
    def unapply(arg: Negation): Option[Formula] = Some(arg.formula)
  }

  val Not = new NegationFactory()
  val ReallyNot = new ReallyNegationFactory()
  val DefinitelyNot = new ReallyNegationFactory()
  val AbsolutelyNot = new ReallyNegationFactory()

  def If (formula: Formula) = new IfBuilder(formula)

  class IfBuilder(antecedent: Formula) {
    def Then(formula: Formula) = Conditional(antecedent, formula)
  }

  val IfThen = new ConditionalFactory

  class AndBuilder(left: Formula) {
    def And (right: Formula) = Conjunction(left, right)
    def & (right: Formula) = Conjunction(left, right)
  }

  def Both (formula: Formula) = new AndBuilder(formula)

  // creator methods
  object GenericConstantBuilder {

    def thing = domain.newConstant
    def person = domain.newConstant

    def relation = domain.newRelation
    def relationship = domain.newRelation
    def property = domain.newRelation

    def sentence = domain.newSentence

  }

  object GenericVariableBuilder {
    def thing = domain.newVariable
    def person = domain.newVariable
    def sentence = domain.oldSentence
  }

  def a = GenericConstantBuilder
  def an = GenericConstantBuilder

  def some = GenericVariableBuilder

  def something = domain.newVariable
  def someone = domain.newVariable

  def not(formula: Formula) = Negation(formula)
  def negate(formula: Formula) = Negation(formula)


}
