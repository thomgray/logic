package com.gray.logic.formula


object FormulaWriter_Alphabetic extends FormulaWriter {
  // connectives
  override def writeNegation(formula: String): String = s"¬$formula"

  override def writeConjunction(formula1: String, formula2: String): String = s"($formula1∧$formula2)"

  override def writeDisjunction(formula1: String, formula2: String): String = s"($formula1∨$formula2)"

  override def writeConditional(formula1: String, formula2: String): String = s"($formula1→$formula2)"

  override def writeBiconditional(formula1: String, formula2: String): String = s"($formula1⟷$formula2)"

  // quantifiers
  override def writeQuantifierUniversal(formula1: String, variable: String): String = s"∀$variable($formula1)"

  override def writeQuantifierExistential(formula1: String, variable: String): String = s"∃$variable($formula1)"

  // atomic formulas
  override def writeSentence(index: Int): String = ('A'.toInt+index).toChar.toString

  override def writeRelation(index: Int, args: Seq[String]): String = {
    val relSymbol = if (index < 9) ('R'.toInt+index).toChar.toString else ('R'.toInt+8-index).toChar.toString
    val argsString = args.mkString
    s"$relSymbol$argsString"
  }

  override def writeEquals(arg1: String, arg2: String): String = s"$arg1=$arg2"

  // terms
  override def writeVariable(index: Int): String =
    if (index<5) ('v'.toInt+index).toChar.toString
    else ('v'.toInt+4-index).toChar.toString

  override def writeConstant(index: Int): String = ('a'.toInt+index).toChar.toString

  override def writeFunction(index: Int, args: Seq[String]): String = {
    val f = ('f'.toInt+index).toChar.toString
    val argsString = args.mkString(",")
    s"$f($argsString)"
  }
}