package com.gray.logic.formula.language

import com.gray.logic.formula.elements._
import com.gray.logic.formula.elements.LConnectiveType._

object LSyntaxStandard extends LSyntax{
  override def syntaxForFunction(token: String, args: Seq[String], lFunction: LFunction): String =
    s"$token(${args.mkString})"

  //formulas
  override def syntaxForRelation(token: String, args: Seq[String], lRelation: LRelation): String = s"$token${args.mkString}"

  override def syntaxForEquals(token: String, leftTerm: String, rightTerm: String, lEquals: LEquals): String = s"$leftTerm$token$rightTerm"

  //compositors
  override def syntaxForConnective(token: String, args: Seq[String], lConnective: LConnective): String = lConnective.connectiveType match {
    case Negation => s"$token${args(0)}"
    case _ => s"(${args(0)}$token${args(1)})"
  }

  override def syntaxForQuantifier(token: String, variable: String, formula: String, lQuantifier: LQuantifier): String = s"$token$variable$formula"


  private def binaryConnective(connective: String, formulas: Seq[String]) = Seq(
    "(", formulas.head, connective, formulas.last, ")"
  )
  
  override def syntacticRuleConjunction(connective: String, formulas: Seq[String]): Seq[String] = binaryConnective(connective, formulas)

  override def syntacticRuleDisjunction(connective: String, formulas: Seq[String]): Seq[String] =  binaryConnective(connective, formulas)
}
