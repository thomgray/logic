package com.gray.logic.language

trait FormulaWriterAlphabetic extends FormulaWriter {

  private def aInt = 'a'.toInt

  private def AInt = 'A'.toInt

  private def vInt = 'v'.toInt

  private def fInt = 'f'.toInt

  private def RInt = 'R'.toInt

  override def writeSentence(index: Int): String = (AInt + index).toChar.toString

  override def writeRelation(index: Int, args: Seq[String]): String = {
    val token = if (index < 9) (RInt + index).toChar.toString else (RInt-index+8).toChar.toString
    token + args.mkString
  }

  override def writeEquals(left: String, right: String): String = s"$left=$right"

  override def writeConjunction(left: String, right: String): String = s"($left⋀$right)"

  override def writeDisjunction(left: String, right: String): String = s"($left⋁$right)"

  override def writeConditional(left: String, right: String): String = s"($left⟶ $right)"

  override def writeBiconditional(left: String, right: String): String = s"($left⟷ $right)"

  override def writeNegation(formula: String): String = s"¬$formula"

  override def writeUniversalQuantifier(variable: String, formula: String): String = s"∀$variable$formula"

  override def writeExistentialQuantifier(variable: String, formula: String): String = s"∃$variable$formula"

  override def writeConstant(index: Int): String = (aInt + index).toChar.toString

  override def writeVariable(index: Int): String = if (index < 5) {
    (vInt + index).toChar.toString
  } else (vInt - index + 4).toChar.toString

  override def writeFunction(index: Int, terms: Seq[String]): String =
    (fInt + index).toChar.toString + "("+ terms.mkString +")"
}

object FormulaWriterAlphabetic extends FormulaWriterAlphabetic
