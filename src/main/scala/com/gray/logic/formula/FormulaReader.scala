package com.gray.logic.formula

import com.gray.logic.formula.elements._
import com.gray.logic.formula.elements.LConnectiveType._
import com.gray.logic.formula.elements.LQuantifierType._
import com.gray.logic.mixin.ReadUtils

trait FormulaReader extends ReadUtils {

  def before(string: String) = string

  def read(string: String) = readRecursive(before(string))

  private def readRecursive(string: String): LFormula = {
    // compositors
    readConnective(string) match {
      case Some((connectiveType, sequence)) =>
        val formulas = sequence map readRecursive
        return new LFormula(LConnective(connectiveType), formulas)
      case _ =>
    }

    readQuantifier(string) match {
      case Some((connective, variable, remainder)) =>
        val f1 = readRecursive(remainder)
        val quant = new LQuantifier(connective, LVariable(variable))
        return new LFormula(quant, Seq(f1))
      case _ =>
    }

    // atomic formulas SL
    readSentence(string) match {
      case Some(i) => return LFormula(LSentence(i))
      case _ =>
    }

    //atomic formulas PL
    readRelation(string) match {
      case Some((i, args)) =>
        val argTerms = args map readTerm
        val relation = LRelation(i, argTerms)
        return LFormula(relation)
      case _ =>
    }

    readEquals(string) match {
      case Some((left, right)) =>
        val eq = LEquals(readTerm(left), readTerm(right))
        return LFormula(eq)
      case _ =>
    }
    throw new Exception(s"Could not read a formula from $string")
  }

  def readTerm(string: String): LTerm = {
    readConstant(string) match {
      case Some(i) => return LConstant(i)
      case _ =>
    }
    readVariable(string) match {
      case Some(i) => return LVariable(i)
      case _ =>
    }
    readFunction(string) match {
      case Some((i, argStrings)) =>
        val argTerms = argStrings map readTerm
        return LFunction(i, argTerms)
      case _ =>
    }
    throw new Exception(s"Could not read a term from $string")
  }



  //compositors
  def readConnective(string: String): Option[(ConnectiveType, Seq[String])]

  def readQuantifier(string: String): Option[(QuantifierType, Int, String)]

  //atomic formulas
  def readSentence(string: String): Option[Int]

  def readEquals(string: String): Option[(String, String)]

  def readRelation(string: String): Option[(Int, Seq[String])]

  //terms
  def readFunction(string: String): Option[(Int, Seq[String])]

  def readConstant(string: String): Option[Int]

  def readVariable(string: String): Option[Int]

}


