package com.gray.logic.utils

import com.gray.logic.formula.Formula
import com.gray.logic.language.FormulaReaderAlphabetic

object FormulaLibrary {
  implicit val reader = FormulaReaderAlphabetic

  val `A` = Formula.read("A").get
  val `B` = Formula.read("B").get
  val `A->B` = Formula.read("(A->B)").get

}
