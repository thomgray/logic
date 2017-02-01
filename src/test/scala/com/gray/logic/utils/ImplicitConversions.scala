package com.gray.logic.utils

import com.gray.logic.deduction.DeductionSequence
import com.gray.logic.deduction.inference.{DeductionRequest, DeductionStackDud}
import com.gray.logic.formula.Formula
import com.gray.logic.language.{FormulaReaderAlphabetic, FormulaWriterAlphabetic}

trait ImplicitConversions {
  import scala.language.implicitConversions

  private val writer = FormulaWriterAlphabetic
  private val reader = FormulaReaderAlphabetic

  implicit def tupleToDedRequest(tuple: (Formula, DeductionSequence)): DeductionRequest = DeductionRequest(tuple._1, tuple._2, DeductionStackDud)
  implicit def toDedRequest(tuple: (Formula, Seq[Formula])): DeductionRequest = DeductionRequest(tuple._1, DeductionSequence(tuple._2: _*), DeductionStackDud)

  implicit def stringToFormula(string: String): Formula = Formula.read(string)(reader).get
  implicit def formulaToString(formula: Formula): String = formula.write(writer)

}
