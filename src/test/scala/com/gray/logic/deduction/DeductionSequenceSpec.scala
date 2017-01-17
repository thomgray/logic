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
    deduction.addAssumptionCP(s1)
    deduction.tier shouldBe 1
  }

  "addCP" should "decrement the tier" in {
    val deduction = DeductionSequence(s0)
    deduction.tier shouldBe 0
    val (node2, deduction2) = deduction.addAssumptionCP(s1)
    deduction2.tier shouldBe 1
    val (node3, deduction3) = deduction2.addCP(s1, node2, node2)
    node3.tier shouldBe 0
    deduction3.tier shouldBe 0
  }

  "visible nodes" should "return all nodes if all nodes are in the same tier" in {
    val nodes = Seq(Sentence(0), Sentence(1), Sentence(2), Sentence(3))
    val sequence = DeductionSequence(nodes:_*)

    sequence.visibleNodes.map(_.formula) shouldBe nodes
  }

  it should "return all nodes visible from the top" in {
    val p1 = Sentence(0)
    val a1 = Sentence(1)
    val a2 = Sentence(2)
    val deduction = DeductionSequence(p1)
    val deduction2 = deduction.addAssumptionCP(a1)._2
    val deduction3 = deduction2.addAssumptionCP(a2)._2

    val visibles = deduction.visibleNodes
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

    deduction.visibleNodes.map(_.formula) shouldBe Seq(s0, s1, s2)

    deduction.nodes.append(cpNode)
    deduction.tier = 0

    deduction.visibleNodes.map(_.formula) shouldBe Seq(s0, cpFormula)
  }

}
