package cucumber.steps

import com.gray.logic.deduction.{Deduction, DeductionSequence, DeductionSuccess}
import com.gray.logic.deduction.inference.InferenceHard
import com.gray.logic.formula.{Formula, Sentence}
import com.gray.logic.language.{FormulaReaderAlphabetic, FormulaWriterAlphabetic}

class DeductionSteps extends DeductionBaseSteps {

  import DeductionHolder._

  implicit val writer = FormulaWriterAlphabetic
  implicit val reader = FormulaReaderAlphabetic

  Given("""^a deduction exists with premises "([^"]*)"$""") { (arg0: String) =>
    premises = arg0.split(",").toSeq.map(_.trim).flatMap(f => Formula.read(f))
  }

  When("""^I attempt to prove "([^"]*)"$""") { (arg0: String) =>
    conclusion = Formula.read(arg0).get
    deduction = new Deduction(conclusion, premises) with InferenceHard
    deduction.prove(conclusion) match {
      case Some((node, seq)) =>
        conclusionNode = Some(node)
        deductionSequence = seq
      case None =>
    }
  }

  Then("""^the deduction succeeds$""") { () =>
    conclusionNode shouldBe defined
    conclusionNode.get.formula shouldBe conclusion
  }

  Then("""^show me the deduction$"""){ () =>
    println(deduction.write)
  }



}
