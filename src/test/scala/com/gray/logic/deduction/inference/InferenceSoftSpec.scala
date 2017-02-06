package com.gray.logic.deduction.inference

import com.gray.logic.deduction.{DeductionNode, DeductionSequence, InferenceRule}
import com.gray.logic.formula._
import com.gray.logic.language.FormulaWriterAlphabetic
import com.gray.logic.utils.ImplicitConversions
import org.scalatest.{FlatSpec, Matchers}

class InferenceSoftSpec extends FlatSpec with Matchers with InferenceSoft with ImplicitConversions{

  "inferMP" should "succeed if the conditional and the antecedent are in the deduction" in {
    val conditional = Conditional(Sentence(0), Sentence(1))
    val antecedent = Sentence(0)
    val consequence = Sentence(1)

    val premiseSequence = DeductionSequence(conditional, antecedent)
    inferMP_Soft(DeductionRequest(consequence, premiseSequence, DeductionStackDud)) match {
      case Proven(node, seq) =>
        seq.length shouldBe 3
        node.formula shouldBe consequence
        node.inferenceRule shouldBe InferenceRule.MP
      case Unproven => fail
    }
  }

  it should "fail if the conditional is not in the deduction" in {
    val antecedent = Sentence(0)
    val consequence = Sentence(1)

    inferMP_Soft(DeductionRequest(consequence, DeductionSequence(antecedent), DeductionStackDud)) shouldBe Unproven
  }

  it should "fail if the antecedent is not in the deduction" in {
    val conditional = Conditional(Sentence(0), Sentence(1))
    val consequence = Sentence(1)

    inferMP_Soft(DeductionRequest(consequence, DeductionSequence(conditional), DeductionStackDud)) shouldBe Unproven
  }

  "inferDI" should "succeed if one of the disjunction is in the deduction" in {
    val dj1 = Sentence(0)
    val conclusion = Disjunction(dj1, Sentence(1))

    val initial = DeductionSequence(dj1)

    inferDI_Soft(DeductionRequest(conclusion, initial, DeductionStackDud)) match {
      case Proven(node, seq) =>
        node.formula shouldBe conclusion
        node.inferenceRule shouldBe InferenceRule.DI
      case _ => fail
    }
  }


  it should "fail if neither disjunct is in the deduction" in {
    val dj1 = Sentence(0)
    val conclusion = Disjunction(Sentence(2), Sentence(1))

    inferDI_Soft(conclusion, DeductionSequence(dj1)) shouldBe Unproven
  }


  "inferCI" should "succeed if both conjuncts are in the deduction" in {
    val c1 = Sentence(0)
    val c2 = Sentence(1)
    val conjunction = Conjunction(c1, c2)

    inferCI_Soft(conjunction, Seq(c1, c2)) match {
      case Proven(conclusion, seq) =>
        conclusion.formula shouldBe conjunction
        conclusion.inferenceRule shouldBe InferenceRule.CI
      case Unproven => fail
    }
  }

  it should "fail if either conjunct is not in the deduction" in {
    val c1 = Sentence(0)
    val c2 = Sentence(1)
    val conjunction = Conjunction(c1, c2)

    inferCI_Soft(conjunction, Seq(c1)) shouldBe Unproven
  }

  "inferCE" should "succeed if am appropraite conjunction is in the deduction" in {
    val c1 = Sentence(0)
    val c2 = Sentence(1)
    val conjunction = Conjunction(c1, c2)

    inferCE_Soft(c1, Seq(conjunction)) match {
      case Proven(DeductionNode(formula, rule), _) =>
        formula shouldBe c1
        rule shouldBe InferenceRule.CE
      case _ => fail
    }
  }

  it should "fail if an appropriate conjunction is not in the deduction" in {
    val c1 = Sentence(0)
    val c2 = Sentence(1)
    val conjunction = Conjunction(Sentence(3), c2)

    inferCE_Soft(c1, Seq(conjunction)) shouldBe Unproven
  }


  "inferBI" should "succeed if both conditionals are in the deduciton" in {
    val c1 = Conditional(Sentence(0), Sentence(1))
    val c2 = Conditional(Sentence(1), Sentence(0))
    val biconditional = Biconditional(Sentence(0), Sentence(1))

    inferBI_Soft(biconditional, Seq(c1, c2)) match {
      case Proven(DeductionNode(formula, rule), _) =>
        formula shouldBe biconditional
        rule shouldBe InferenceRule.BI
      case _ => fail
    }
  }

  it should "fail if either conditional is not in the deduction" in {
    val c1 = Conditional(Sentence(0), Sentence(1))
    val biconditional = Biconditional(Sentence(0), Sentence(1))

    inferBI_Soft(biconditional, Seq(c1)) shouldBe Unproven
  }

  "inferBE" should "succeed if an appropriate biconditional is in the deduction" in {
    val c1 = Conditional(Sentence(0), Sentence(1))
    val biconditional = Biconditional(Sentence(0), Sentence(1))

    inferBE_Soft(c1, Seq(biconditional)) match {
      case Proven(DeductionNode(conclusion, rule), _) =>
        conclusion shouldBe c1
        rule shouldBe InferenceRule.BE
      case _ => fail
    }
  }

  it should "fail if no appropriate biconditional is in the deduction" in {
    val c1 = Conditional(Sentence(0), Sentence(2))
    val biconditional = Biconditional(Sentence(0), Sentence(1))

    inferBE_Soft(c1, Seq(biconditional)) shouldBe Unproven
  }

  "inferDNI" should "succeed if the dne of the conclusion is in the deduction" in {
    val premise = Sentence(0)

    val dni = Negation(Negation(premise))

    inferDNI_Soft(dni, Seq(premise)) match {
      case Proven(DeductionNode(conclusion, InferenceRule.DNI), _) =>
        conclusion shouldBe dni
      case _ => fail
    }
  }

  it should "fail if the dne of the conclusion is not in the deduction" in {
    val premise = Sentence(0)
    val dni = Negation(Negation(Sentence(1)))

    inferDNI_Soft(dni, Seq(premise)) shouldBe Unproven
  }

  it should "fail if the conclusion is not a double negation" in {
    val premise = Sentence(0)
    val dni = Negation(Sentence(0))

    inferDNI_Soft(dni, Seq(premise)) shouldBe Unproven
  }

  "inferDNE" should "succeed if the dni of the conclusion is in the deduction" in {
    val conclusion = Sentence(0)
    val premise = Negation(Negation(conclusion))

    inferDNE_Soft(conclusion, Seq(premise)) match {
      case Proven(DeductionNode(conc, InferenceRule.DNE), _) =>
        conc shouldBe conclusion
      case _ => fail
    }
  }

  it should "fail if the dni of the conclusion is not in the deduction" in {
    val conclusion = Sentence(0)
    val premise = Negation(Negation(Sentence(1)))

    inferDNE_Soft(conclusion, Seq(premise)) shouldBe Unproven
  }
}