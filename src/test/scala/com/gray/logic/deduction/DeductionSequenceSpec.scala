package com.gray.logic.deduction

import com.gray.logic.formula.{Conditional, Sentence}
import org.scalatest.{FlatSpec, Matchers}

class DeductionSequenceSpec extends FlatSpec with Matchers {

  val s0 = Sentence(0)
  val s1 = Sentence(1)
  val s2 = Sentence(2)

  "addAssumption" should "increment the tier" in {
    val deduction = DeductionSequence(s0)
    deduction.tier shouldBe 0
    val deduction2 = deduction.addAssumptionCP(s1).sequence
    deduction2.tier shouldBe 1
  }

  "addCP" should "decrement the tier" in {
    val deduction = DeductionSequence(s0)
    deduction.tier shouldBe 0
    val proven1 = deduction.addAssumptionCP(s1)
    proven1.sequence.tier shouldBe 1
    val proven2 = proven1.sequence.addCP(s1, proven1.deductionNode, proven1.deductionNode)
    proven2.deductionNode.tier shouldBe 0
    proven2.sequence.tier shouldBe 0
  }

  "visible nodes" should "return all nodes if all nodes are in the same tier in reverse order" in {
    val nodes = Seq(Sentence(0), Sentence(1), Sentence(2), Sentence(3))
    val sequence = DeductionSequence(nodes:_*)

    sequence.visibleNodes.map(_.formula) shouldBe nodes.reverse
  }

  it should "return all nodes visible from the top" in {
    val p1 = Sentence(0)
    val a1 = Sentence(1)
    val a2 = Sentence(2)
    val deduction = DeductionSequence(p1)
    val deduction2 = deduction.addAssumptionCP(a1).sequence
    val deduction3 = deduction2.addAssumptionCP(a2).sequence

    val visibles = deduction3.visibleNodes.reverse
    visibles.length shouldBe 3
    visibles(0).formula shouldBe p1
    visibles(0).tier shouldBe 0
    visibles(1).formula shouldBe a1
    visibles(1).tier shouldBe 1
    visibles(2).formula shouldBe a2
    visibles(2).tier shouldBe 2
  }

  it should "filter out hidden nodes" in {
    val cpFormula = Conditional(s1,s2)
    val premise = DeductionNode(s0, InferenceRule.Premise, Nil)
    val cpAss = DeductionNode(s1, InferenceRule.AssCP, Nil, 1)
    val cpConc = DeductionNode(s2, InferenceRule.CE, Seq(cpAss), 1)
    val cpNode = DeductionNode(cpFormula, InferenceRule.CP, Seq(cpAss, cpConc), 0)

    val deduction = new DeductionSequence(Seq(premise, cpAss, cpConc), 1)

    deduction.visibleNodes.map(_.formula) shouldBe Seq(s0, s1, s2).reverse

    val newSeq = deduction.appendLine(cpNode)._2.stepDown

    newSeq.tier shouldBe 0

    newSeq.visibleNodes.map(_.formula) shouldBe Seq(cpFormula, s0)
  }

}
