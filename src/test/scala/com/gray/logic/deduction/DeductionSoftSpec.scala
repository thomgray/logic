package com.gray.logic.deduction

import com.gray.logic.formula.{FormulaWriter_Alphabetic, LFormula}
import com.gray.logic.formula.elements.LSentence
import org.scalatest.{FlatSpec, Matchers}

class DeductionSoftSpec extends FlatSpec with Matchers {

  implicit val writer = FormulaWriter_Alphabetic

  "MP_Soft" should "infer MP if the antecedent and conditional are in the deduction" in {
    val conclusion = LFormula(LSentence(0))
    val antecedent = LFormula(LSentence(1))
    val conditional = LFormula.conditional(antecedent, conclusion)

    val deduction = new DeductionSoft(conclusion, antecedent, conditional)

    println(deduction.write)

    deduction.MP_Soft(conclusion) match {
      case Some(node) =>
        node.formula shouldBe conclusion
        node.inferenceRule shouldBe InferenceRules.ModusPonens
      case None => fail()
    }

    println(deduction.write)
  }
}
