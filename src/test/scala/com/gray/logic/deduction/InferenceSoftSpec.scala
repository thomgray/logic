package com.gray.logic.deduction

import com.gray.logic.deduction.inference.InferenceSoft
import com.gray.logic.formula._
import com.gray.logic.language.{FormulaWriterAlphabetic, HumanReadable}
import org.scalatest.{FlatSpec, Matchers}

class InferenceSoftSpec extends FlatSpec with Matchers with InferenceSoft {

  implicit val writer = FormulaWriterAlphabetic
  import scala.language.implicitConversions

  implicit def tupleToDedRequest(tuple: (Formula, DeductionSequence)): DeductionRequest = DeductionRequest(tuple._1, tuple._2, DeductionStackDud)

  implicit def toDedRequest(tuple: (Formula, Seq[Formula])): DeductionRequest = DeductionRequest(tuple._1, DeductionSequence(tuple._2: _*), DeductionStackDud)

  "inferMP" should "succeed if the conditional and the antecedent are in the deduction" in {
    val conditional = Conditional(Sentence(0), Sentence(1))
    val antecedent = Sentence(0)
    val consequence = Sentence(1)

    val premiseSequence = DeductionSequence(conditional, antecedent)
    inferMP_Soft(DeductionRequest(consequence, premiseSequence, DeductionStackDud)) match {
      case DeductionSuccess(node, seq, stack) =>
        seq.length shouldBe 3
        node.formula shouldBe consequence
        node.inferenceRule shouldBe InferenceRule.MP
      case DeductionFailure => fail
    }
  }

  it should "fail if the conditional is not in the deduction" in {
    val antecedent = Sentence(0)
    val consequence = Sentence(1)

    inferMP_Soft(DeductionRequest(consequence, DeductionSequence(antecedent), DeductionStackDud)) shouldBe DeductionFailure
  }

  it should "fail if the antecedent is not in the deduction" in {
    val conditional = Conditional(Sentence(0), Sentence(1))
    val consequence = Sentence(1)

    inferMP_Soft(DeductionRequest(consequence, DeductionSequence(conditional), DeductionStackDud)) shouldBe DeductionFailure
  }

  "inferDI" should "succeed if one of the disjunction is in the deduction" in {
    val dj1 = Sentence(0)
    val conclusion = Disjunction(dj1, Sentence(1))

    val initial = DeductionSequence(dj1)

    inferDI_Soft(DeductionRequest(conclusion, initial, DeductionStackDud)) match {
      case DeductionSuccess(node, seq, _) =>
        node.formula shouldBe conclusion
        node.inferenceRule shouldBe InferenceRule.DI
      case _ => fail
    }
  }


  it should "fail if neither disjunct is in the deduction" in {
    val dj1 = Sentence(0)
    val conclusion = Disjunction(Sentence(2), Sentence(1))

    inferDI_Soft(conclusion, DeductionSequence(dj1)) shouldBe DeductionFailure
  }


  "inferCI" should "succeed if both conjuncts are in the deduction" in {
    val c1 = Sentence(0)
    val c2 = Sentence(1)
    val conjunction = Conjunction(c1, c2)

    inferCI_Soft(conjunction, Seq(c1, c2)) match {
      case DeductionSuccess(conclusion, seq, _) =>
        conclusion.formula shouldBe conjunction
        conclusion.inferenceRule shouldBe InferenceRule.CI
      case DeductionFailure => fail
    }
  }

  it should "fail if either conjunct is not in the deduction" in {
    val c1 = Sentence(0)
    val c2 = Sentence(1)
    val conjunction = Conjunction(c1, c2)

    inferCI_Soft(conjunction, Seq(c1)) shouldBe DeductionFailure
  }

  "inferCE" should "succeed if am appropraite conjunction is in the deduction" in {
    val c1 = Sentence(0)
    val c2 = Sentence(1)
    val conjunction = Conjunction(c1, c2)

    inferCE_Soft(c1, Seq(conjunction)) match {
      case DeductionSuccess(DeductionNode(formula, rule), _, _) =>
        formula shouldBe c1
        rule shouldBe InferenceRule.CE
      case _ => fail
    }
  }

  it should "fail if an appropriate conjunction is not in the deduction" in {
    val c1 = Sentence(0)
    val c2 = Sentence(1)
    val conjunction = Conjunction(Sentence(3), c2)

    inferCE_Soft(c1, Seq(conjunction)) shouldBe DeductionFailure
  }


  "inferBI" should "succeed if both conditionals are in the deduciton" in {
    val c1 = Conditional(Sentence(0), Sentence(1))
    val c2 = Conditional(Sentence(1), Sentence(0))
    val biconditional = Biconditional(Sentence(0), Sentence(1))

    inferBI_Soft(biconditional, Seq(c1, c2)) match {
      case DeductionSuccess(DeductionNode(formula, rule), _, _) =>
        formula shouldBe biconditional
        rule shouldBe InferenceRule.BI
      case _ => fail
    }
  }

  it should "fail if either conditional is not in the deduction" in {
    val c1 = Conditional(Sentence(0), Sentence(1))
    val biconditional = Biconditional(Sentence(0), Sentence(1))

    inferBI_Soft(biconditional, Seq(c1)) shouldBe DeductionFailure
  }

  "inferBE" should "succeed if an appropriate biconditional is in the deduction" in {
    val c1 = Conditional(Sentence(0), Sentence(1))
    val biconditional = Biconditional(Sentence(0), Sentence(1))

    inferBE_Soft(c1, Seq(biconditional)) match {
      case DeductionSuccess(DeductionNode(conclusion, rule), _, _) =>
        conclusion shouldBe c1
        rule shouldBe InferenceRule.BE
      case _ => fail
    }
  }

  it should "fail if no appropriate biconditional is in the deduction" in {
    val c1 = Conditional(Sentence(0), Sentence(2))
    val biconditional = Biconditional(Sentence(0), Sentence(1))

    inferBE_Soft(c1, Seq(biconditional)) shouldBe DeductionFailure
  }

  "inferDNI" should "succeed if the dne of the conclusion is in the deduction" in {
    val premise = Sentence(0)

    val dni = Negation(Negation(premise))

    inferDNI_Soft(dni, Seq(premise)) match {
      case DeductionSuccess(DeductionNode(conclusion, InferenceRule.DNI), _, _) =>
        conclusion shouldBe dni
      case _ => fail
    }
  }

  it should "fail if the dne of the conclusion is not in the deduction" in {
    val premise = Sentence(0)
    val dni = Negation(Negation(Sentence(1)))

    inferDNI_Soft(dni, Seq(premise)) shouldBe DeductionFailure
  }

  it should "fail if the conclusion is not a double negation" in {
    val premise = Sentence(0)
    val dni = Negation(Sentence(0))

    inferDNI_Soft(dni, Seq(premise)) shouldBe DeductionFailure
  }

  "inferDNE" should "succeed if the dni of the conclusion is in the deduction" in {
    val conclusion = Sentence(0)
    val premise = Negation(Negation(conclusion))

    inferDNE_Soft(conclusion, Seq(premise)) match {
      case DeductionSuccess(DeductionNode(conc, InferenceRule.DNE), _, _) =>
        conc shouldBe conclusion
      case _ => fail
    }
  }

  it should "fail if the dni of the conclusion is not in the deduction" in {
    val conclusion = Sentence(0)
    val premise = Negation(Negation(Sentence(1)))

    inferDNE_Soft(conclusion, Seq(premise)) shouldBe DeductionFailure
  }
}