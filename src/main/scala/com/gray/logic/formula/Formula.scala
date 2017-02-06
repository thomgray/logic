package com.gray.logic.formula

import com.gray.logic.language.{FormulaReader, FormulaWriter}

trait Formula {
  def negationStrict() = Negation(this)

  def negation() = this match {
    case Negation(f) => f
    case _ => Negation(this)
  }

  def write(implicit formulaWriter: FormulaWriter) = formulaWriter(this)

  def isComposite = this.isInstanceOf[Compositor]

  def isAtomic = !isComposite

  def decompositions : Seq[Formula] = this match {
    case f: AtomicFormula => Seq(f)
    case f @ Connective(decomps) => f +: decomps.flatMap(_.decompositions)
    case f @ Quantifier(_, decomp) => f +: decomp.decompositions
  }
}

object Formula {
  def read(string: String)(implicit formulaReader: FormulaReader) = formulaReader.read(string)
}
