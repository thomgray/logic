package com.gray.logic.formula

import com.gray.logic.formula.elements._
import com.gray.logic.formula.elements.LConnectiveType._
import com.gray.logic.formula.elements.LQuantifierType._


trait FormulaWriter {

  protected[formula] def write(formula: LFormula): String = formula.root match {
    case compositor: LCompositor => writeCompositor(compositor, formula.composition.map(write))
    case LSentence(i) => writeSentence(i)
    case LRelation(i, args) => writeRelation(i, args.map(writeTerm))
    case LEquals(t1, t2) => writeEquals(writeTerm(t1), writeTerm(t2))
    case _ => throw new UnsupportedOperationException()
  }

  protected [formula] def writeCompositor(compositor: LCompositor, formulas : Seq[String]) = compositor match {
    case LConnective(connectiveType) => connectiveType match {
      case Conjunction => writeConjunction(formulas(0), formulas(1))
      case Conditional => writeConditional(formulas(0), formulas(1))
      case Disjunction => writeDisjunction(formulas(0), formulas(1))
      case Biconditional => writeBiconditional(formulas(0), formulas(1))
      case Negation => writeNegation(formulas(0))
    }
    case LQuantifier(quantifierType, LVariable(i)) => quantifierType match {
      case Existential => writeQuantifierExistential(formulas(0), writeVariable(i))
      case Universal => writeQuantifierUniversal(formulas(0), writeVariable(i))
    }
  }

  protected [formula] def writeTerm(term: LTerm): String = term match {
    case LConstant(i) => writeConstant(i)
    case LVariable(i) => writeVariable(i)
    case LFunction(i, args) => writeFunction(i, args.map(writeTerm))
    case _ => throw new UnsupportedOperationException("Terms are onlt constants, variables or functions!")
  }

  // connectives
  def writeNegation(formula: String): String
  def writeConjunction(formula1: String, formula2: String): String
  def writeDisjunction(formula1: String, formula2: String): String
  def writeConditional(formula1: String, formula2: String): String
  def writeBiconditional(formula1: String, formula2: String): String
  // quantifiers
  def writeQuantifierUniversal(formula1: String, variable: String): String
  def writeQuantifierExistential(formula1: String, variable: String): String

  // atomic formulas
  def writeSentence(index: Int): String
  def writeRelation(index: Int, args: Seq[String]): String
  def writeEquals(arg1: String, arg2: String): String

  // terms
  def writeVariable(index: Int): String
  def writeConstant(index: Int): String
  def writeFunction(index: Int, args: Seq[String]): String

}