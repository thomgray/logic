package com.gray.logic.language

import com.gray.logic
import com.gray.logic.formula
import com.gray.logic.formula._

trait Language {

  protected def tokenForTerm(term: Term) = term match {
    case constant: Constant => tokenForConstant(constant)
    case variable: Variable => tokenForVariable(variable)
    case function: Function => tokenForFunction(function)
  }

  protected def tokenForFormula(formula: Formula) = formula match {
    case sentence: Sentence => tokenForSentence(sentence)
    case relation: Relation => tokenForRelation(relation)
    case equals: logic.formula.Equals => tokenForEquals(equals)
    case connective: Connective => tokenForConnective(connective)
    case quantifier: Quantifier => tokenForQuantifier(quantifier)
  }

  //terms
  def tokenForVariable(variable: Variable): String
  def tokenForConstant(constant: Constant): String
  def tokenForFunction(function: Function): String

  //formulas
  def tokenForSentence(sentence: Sentence): String
  def tokenForRelation(relation: Relation): String
  def tokenForEquals(equals: formula.Equals): String

  //compositors
  def tokenForConnective(connective: Connective): String
  def tokenForQuantifier(quantifier: Quantifier): String



  ///reading

  //formulas
  def readSentenceToken(string: String, at: Int): Option[(Int, StringSegments)]
  def readRelationToken(string: String, at: Int): Option[(Int, StringSegments)]
  def readEqualsToken(string: String, at: Int): Option[StringSegments]

  //compositors
  def readConnectiveToken(string: String, at: Int): Option[(ConnectiveType.Value, StringSegments)]
  def readQuantifierToken(string: String, at: Int): Option[(QuantifierType.Value, StringSegments)]

  //terms
  def readFunctionToken(string: String, at: Int): Option[(Int, StringSegments)]
  def readConstantToken(string: String, at: Int): Option[(Int, StringSegments)]
  def readVariableToken(string: String, at: Int): Option[(Int, StringSegments)]


}

case class StringSegments(left: String, right: String)
object StringSegments {
  def apply(original: String, at: Int, length: Int = 1): StringSegments = new StringSegments(original.substring(0, at), original.substring(at+length))
}
