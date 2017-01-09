package com.gray.logic.formula.language

import com.gray.logic.formula.elements._

trait LSyntax {

  //terms
  def syntaxForConstant(token: String, lConstant: LConstant): String = token
  def syntaxForVariable(token: String, lVariable: LVariable): String = token
  def syntaxForFunction(token: String, args: Seq[String], lFunction: LFunction): String

  //formulas
  def syntaxForRelation(token: String, args: Seq[String], lRelation: LRelation): String
  def syntaxForSentence(token: String, lSentence: LSentence) = token
  def syntaxForEquals(token: String, leftTerm: String, rightTerm: String, lEquals: LEquals): String

  //compositors
  def syntaxForConnective(token: String, args: Seq[String], lConnective: LConnective): String
  def syntaxForQuantifier(token: String, variable: String, formula: String, lQuantifier: LQuantifier): String


  //reading

  def syntacticRuleConjunction(connective: String, formulas: Seq[String]): Seq[String]
  def syntacticRuleDisjunction(connective: String, formulas: Seq[String]): Seq[String]
}
