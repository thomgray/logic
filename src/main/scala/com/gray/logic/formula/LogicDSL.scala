package com.gray.logic.formula

import com.gray.logic.deduction.Deduction

trait LogicDSL {
  import scala.language.implicitConversions

  implicit def stringToTermWrapper(string: String): TermWrapper = new TermWrapper(domain.namedConstant(string), domain)
  implicit def stringToFormulaWrapper(string: String): FormulaWrapper = new FormulaWrapper(domain.namedSentence(string))
  implicit def termWrap(term: Term): TermWrapper = new TermWrapper(term, domain)
  implicit def formulaWrap(formula: Formula): FormulaWrapper = new FormulaWrapper(formula)
  implicit def stringToConstant(string: String): Constant = domain.namedConstant(string)
  implicit def stringToSentence(string: String): Sentence = domain.namedSentence(string)
//  implicit def stringWrap(string: String): StringWrapper = new StringWrapper(string, domain)

  implicit private val domain = new Domain

  def something = domain.newConstant
  def someone = domain.newConstant
  def anything = domain.newVariable

  def a = new AWord(domain)
  def an = new AWord(domain)
  def some = new AWord(domain)
  def any = new AnyWord(domain)

  def that(string: String) = domain.namedSentence(string)

  def not(formula: Formula) = Negation(formula)
  def ~(formula: Formula) = not(formula)

  def If(formula: Formula) = new IfBuilder(formula)
  def When(formula: Formula) = new IfBuilder(formula)

  def Given(formula: Formula*) = new GivenWrapper(formula)
}

object LogicDSL extends LogicDSL

class AWord(domain: Domain) {
  def sentence = domain.newSentence
  def thing = domain.newConstant
  def person = domain.newConstant
  def relation = domain.newRelation
  def property = domain.newRelation
}

class AnyWord(domain: Domain) {
  def sentence = domain.oldSentence
  def thing = domain.newVariable
  def person = domain.newVariable
}

sealed class TermWrapper(left: Term, domain: Domain) {
  def is(right: RelationScheme) = right(left)
  def is(right: String) = domain.namedRelation(right)(left)
}

sealed class FormulaWrapper(left: Formula) {
  def and(right: Formula) = Conjunction(left, right)
  def or(right: Formula) = Disjunction(left, right)
  def iff(right: Formula) = Biconditional(left, right)
  def ifAndOnlyIf(right: Formula) = Biconditional(left, right)
  def means(right: Formula) = Conditional(left, right)
  def implies(right: Formula) = Conditional(left, right)
  def ->(right: Formula) = Conditional(left, right)
  def &(right: Formula) = Conjunction(left, right)
  def v(right: Formula) = Disjunction(left, right)
}

sealed class GivenWrapper(premises: Seq[Formula]) {
  def prove(formula: Formula) = Deduction.prove(formula, premises)
}

sealed class IfBuilder(left: Formula) {
  def Then(right: Formula) = Conditional(left, right)
}

sealed class StringWrapper(left: String, domain: Domain) {
//  def is(constant: Constant) = {
//    domain.namedConstant(left)
//  }
}