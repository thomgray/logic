package com.gray.logic.main

import com.gray.logic.formula._

object Main {
  implicit val reader = FormulaReader_Alphabetic
  implicit val writer = FormulaWriter_Alphabetic

  def main (args: Array[String]) = {
    val f1 = LFormula.read("(P&Q)")
    val f2 = LFormula.read("(RVS)")

    println(f1.write)
  }
}
