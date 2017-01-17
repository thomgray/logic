package com.gray.logic.formula

import com.gray.logic.language.{FormulaReader, FormulaWriter}

trait Formula {
  def negationStrict() = Negation(this)

  def negation() = this match {
    case Negation(f) => f
    case _ => Negation(this)
  }

  def write(implicit formulaWriter: FormulaWriter) = formulaWriter(this)
}

object Formula {
  def read(string: String)(implicit formulaReader: FormulaReader) = formulaReader.read(string)
}
