package com.gray.logic.deduction

import com.gray.logic.formula.{Conditional, Sentence}
import org.scalatest.{FunSuite, Matchers}

class DeductionNodeTest extends FunSuite with Matchers {

  test("discharge dependencies") {
    val p1 = DeductionNode(Sentence(0), InferenceRule.Premise, Nil)
    val p2 = DeductionNode(Sentence(1), InferenceRule.AssCP, Nil)

    val conc = DeductionNode(Conditional(Sentence(0), Sentence(1)), InferenceRule.CP, Seq(p1, p2))
    val condDischarges = conc.dischargeDependency(p2)

    condDischarges.dependencies shouldBe Seq(p1)
  }

}
