package com.gray.logic.deduction

import com.gray.logic.deduction.inference.InferenceSoft
import com.gray.logic.formula._
import com.gray.logic.language.{FormulaWriterAlphabetic, HumanReadable}
import org.scalatest.{FlatSpec, Matchers}

class InferenceSoftSpec extends FlatSpec with Matchers{

  implicit val writer = FormulaWriterAlphabetic

  "inferMP" should "succeed if the conditional and the antecedent are in the deduction" in {
    val conditional = Conditional(Sentence(0), Sentence(1))
    val antecedent = Sentence(0)
    val consequence = Sentence(1)

    val deduction = new DeductionSoft(consequence, Seq(conditional, antecedent))

    val lastNode = deduction.inferMP_Soft(consequence).get

    deduction.sequence.nodes.length shouldBe 3

    lastNode.formula shouldBe consequence
    lastNode.inferenceRule shouldBe InferenceRule.MP
  }

  it should "fail if the conditional is not in the deduction" in {
    val antecedent = Sentence(0)
    val consequence = Sentence(1)

    val deduction = new DeductionSoft(consequence, Seq(antecedent))
    deduction.inferMP_Soft(consequence)

    deduction.inferMP_Soft(consequence) shouldBe None

    deduction.sequence.nodes.length shouldBe 1
  }

  it should "fail if the antecedent is not in the deduction" in {
    val conditional = Conditional(Sentence(0), Sentence(1))
    val consequence = Sentence(1)

    val deduction = new DeductionSoft(consequence, Seq(conditional))
    deduction.inferMP_Soft(consequence) shouldBe None

    deduction.sequence.nodes.length shouldBe 1
  }

  "inferDI" should "succeed if one of the disjunction is in the deduction" in {
    val dj1 = Sentence(0)
    val conclusion = Disjunction(dj1, Sentence(1))

    val deduction = DeductionSoft(conclusion, dj1)

    deduction.inferDI_Soft(conclusion) match {
      case Some(node) =>
        node.formula shouldBe conclusion
        node.inferenceRule shouldBe InferenceRule.DI
      case None => fail("DI did not succeed")
    }
  }


  it should "fail if neither disjunct is in the deduction" in {
    val dj1 = Sentence(0)
    val conclusion = Disjunction(dj1, Sentence(1))

    val deduction = DeductionSoft(conclusion)
    deduction.inferDI_Soft(conclusion) shouldBe None
  }


  "inferCI" should "succeed if both conjuncts are in the deduction" in {
    val c1 = Sentence(0)
    val c2 = Sentence(1)
    val conjunction = Conjunction(c1,c2)

    val deduction = DeductionSoft(conjunction, c1, c2)
    deduction.inferCI_Soft(conjunction) match {
      case Some(conclusion) =>
        conclusion.formula shouldBe conjunction
        conclusion.inferenceRule shouldBe InferenceRule.CI
      case None => fail()
    }
  }

  it should "fail if either conjunct is not in the deduction" in {
    val c1 = Sentence(0)
    val c2 = Sentence(1)
    val conjunction = Conjunction(c1,c2)

    val deduction = DeductionSoft(conjunction, c1)
    deduction.inferCI_Soft(conjunction) shouldBe None
  }

  "inferCE" should "succeed if am appropraite conjunction is in the deduction" in {
    val c1 = Sentence(0)
    val c2 = Sentence(1)
    val conjunction = Conjunction(c1, c2)

    val deduction = DeductionSoft(c1, conjunction)

    deduction inferCE_Soft c1 match {
      case Some(DeductionNode(formula, rule)) =>
        formula shouldBe c1
        rule shouldBe InferenceRule.CE
      case _ => fail()
    }
  }

  it should "fail if an appropriate conjunction is not in the deduction" in {
    val c1 = Sentence(0)
    val c2 = Sentence(1)
    val conjunction = Conjunction(Sentence(3), c2)

    val deduction = DeductionSoft(c1, conjunction)

    deduction inferCE_Soft c1 shouldBe None
  }


  "inferBI" should "succeed if both conditionals are in the deduciton" in {
    val c1 = Conditional(Sentence(0), Sentence(1))
    val c2 = Conditional(Sentence(1), Sentence(0))
    val biconditional = Biconditional(Sentence(0), Sentence(1))

    val deduction = DeductionSoft(biconditional, c1, c2)

    deduction.inferBI_Soft(biconditional) match {
      case Some(concNode) =>
        concNode.formula shouldBe biconditional
        concNode.inferenceRule shouldBe InferenceRule.BI
      case _ => fail()
    }
  }

  it should "fail if either conditional is not in the deduction" in {
    val c1 = Conditional(Sentence(0), Sentence(1))
    val biconditional = Biconditional(Sentence(0), Sentence(1))

    val deduction = DeductionSoft(biconditional, c1)

    deduction.inferBI_Soft(biconditional) shouldBe None
  }

  "inferBE" should "succeed if an appropriate biconditional is in the deduction" in {
    val c1 = Conditional(Sentence(0), Sentence(1))
    val biconditional = Biconditional(Sentence(0), Sentence(1))

    val deduction = DeductionSoft(c1, biconditional)
    deduction.inferBE_Soft(c1) match {
      case Some(DeductionNode(formula, rule)) =>
        formula shouldBe c1
        rule shouldBe InferenceRule.BE
      case _ => fail
    }
  }

  it should "fail if no appropriate biconditional is in the deduction" in {
    val c1 = Conditional(Sentence(0), Sentence(2))
    val biconditional = Biconditional(Sentence(0), Sentence(1))

    val deduction = DeductionSoft(c1, biconditional)
    deduction.inferBE_Soft(c1) shouldBe None
  }

  "inferDNI" should "succeed if the dne of the conclusion is in the deduction" in {
    val premise = Sentence(0)

    val dni = Negation(Negation(premise))

    val deduction = DeductionSoft(dni, premise)

    deduction.inferDNI_Soft(dni) match {
      case Some(DeductionNode(formula, InferenceRule.DNI)) => formula shouldBe dni
      case _ => fail
    }
  }

  it should "fail if the dne of the conclusion is not in the deduction" in {
    val premise = Sentence(0)
    val dni = Negation(Negation(Sentence(1)))

    val deduction = new Deduction(dni, Seq(premise)) with InferenceSoft

    deduction.inferDNI_Soft(dni) shouldBe None
  }

  it should "fail if the conclusion is not a double negation" in {
    val premise = Sentence(0)
    val dni = Negation(Sentence(0))

    val deduction = DeductionSoft(dni, premise)

    deduction.inferDNI_Soft(dni) shouldBe None
  }

  "inferDNE" should "succeed if the dni of the conclusion is in the deduction" in {
    val conclusion = Sentence(0)
    val premise = Negation(Negation(conclusion))

    val deduction = DeductionSoft(conclusion, premise)
    deduction inferDNE_Soft conclusion match {
      case Some(DeductionNode(formuls, InferenceRule.DNE)) => formuls shouldBe conclusion
      case _ => fail
    }
  }

  it should "fail if the dni of the conclusion is not in the deduction" in {
    val conclusion = Sentence(0)
    val premise = Negation ( Negation (Sentence(1)))

    val deduction = DeductionSoft(conclusion, premise)
    deduction inferDNE_Soft conclusion shouldBe None
  }
}

class DeductionSoft(conclusion: Formula, premises: Seq[Formula]) extends Deduction(conclusion, premises) with InferenceSoft

object DeductionSoft {
//  def apply(conclusion: Formula, premises: Seq[Formula]): DeductionSoft = new DeductionSoft(conclusion, premises)
  def apply(conclusion: Formula, premises: Formula*): DeductionSoft = new DeductionSoft(conclusion, premises)
}
