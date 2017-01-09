package com.gray.logic.formula.language

import com.gray.logic.formula.LFormula
import com.gray.logic.formula.elements.LConnectiveType._
import com.gray.logic.formula.elements._

class LFormulaReader(lLanguage: LLanguage, lSyntax: LSyntax) {

  def before(string: String) = string

  protected[formula] def write(formula: LFormula): String = formula.root match {
    case compositor: LCompositor =>
      val composits = formula.composition map write
      writeCompositor(compositor, composits)
    case sentence@LSentence(i) =>
      val token = lLanguage.tokenForSentence(i)
      lSyntax.syntaxForSentence(token, sentence)
    case rel@LRelation(i, args) =>
      val token = lLanguage.tokenForRelation(i)
      val args = rel.args map writeTerm
      lSyntax.syntaxForRelation(token, args, rel)
    case equals@LEquals(t1, t2) =>
      val token = lLanguage.tokenForEquals()
      lSyntax.syntaxForEquals(token, writeTerm(equals.left), writeTerm(equals.right), equals)
    case _ => throw new UnsupportedOperationException()
  }

  protected [formula] def writeCompositor(compositor: LCompositor, formulas : Seq[String]): String  = compositor match {
    case LConnective(connectiveType) => connectiveType match {
      case Negation => lLanguage.tokenForConnective(connectiveType)+formulas(0)
      case _ => s"(${formulas(0)}"+lLanguage.tokenForConnective(connectiveType)+s"${formulas(1)})"
    }
    case LQuantifier(quantifierType, LVariable(i)) =>
      val token = lLanguage.tokenForQuantifier(quantifierType)
      val variable = lLanguage.tokenForVariable(i)
      s"$token$variable${formulas.head}"
  }

  protected [formula] def writeTerm(term: LTerm): String = term match {
    case LConstant(i) => lLanguage.tokenForConstant(i)
    case LVariable(i) => lLanguage.tokenForVariable(i)
    case LFunction(i, args) =>
      val token = lLanguage.tokenForFunction(i)
      val argString = args map writeTerm
      s"$token($argString)"
    case _ => throw new UnsupportedOperationException("Terms are only constants, variables or functions!")
  }


//  def read(string: String) = readRecursive(before(string))

//  private def readRecursive(string: String): LFormula = {
//    // compositors
//    readConnective(string) match {
//      case Some((connectiveType, sequence)) =>
//        val formulas = sequence map readRecursive
//        return new LFormula(LConnective(connectiveType), formulas)
//      case _ =>
//    }
//
//    readQuantifier(string) match {
//      case Some((connective, variable, remainder)) =>
//        val f1 = readRecursive(remainder)
//        val quant = new LQuantifier(connective, LVariable(variable))
//        return new LFormula(quant, Seq(f1))
//      case _ =>
//    }
//
//    // atomic formulas SL
//    readSentence(string) match {
//      case Some(i) => return LFormula(LSentence(i))
//      case _ =>
//    }
//
//    //atomic formulas PL
//    readRelation(string) match {
//      case Some((i, args)) =>
//        val argTerms = args map readTerm
//        val relation = LRelation(i, argTerms)
//        return LFormula(relation)
//      case _ =>
//    }
//
//    readEquals(string) match {
//      case Some((left, right)) =>
//        val eq = LEquals(readTerm(left), readTerm(right))
//        return LFormula(eq)
//      case _ =>
//    }
//    throw new Exception(s"Could not read a formula from $string")
//  }
//
//  def readTerm(string: String): LTerm = {
//    readConstant(string) match {
//      case Some(i) => return LConstant(i)
//      case _ =>
//    }
//    readVariable(string) match {
//      case Some(i) => return LVariable(i)
//      case _ =>
//    }
//    readFunction(string) match {
//      case Some((i, argStrings)) =>
//        val argTerms = argStrings map readTerm
//        return LFunction(i, argTerms)
//      case _ =>
//    }
//    throw new Exception(s"Could not read a term from $string")
//  }

}
