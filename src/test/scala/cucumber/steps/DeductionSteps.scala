package cucumber.steps

import com.gray.logic.deduction.{Deduction, DeductionSequence, InferenceRule}
import com.gray.logic.deduction.inference.InferenceHard
import com.gray.logic.formula.{Formula, Sentence}
import com.gray.logic.language.{FormulaReaderAlphabetic, FormulaWriterAlphabetic}
import cucumber.api.PendingException

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

import scala.language.implicitConversions
import scala.concurrent.ExecutionContext.Implicits.global

class DeductionSteps extends DeductionBaseSteps {

  import DeductionHolder._

  implicit val writer = FormulaWriterAlphabetic
  implicit val reader = FormulaReaderAlphabetic

  implicit def stringToInferenceRule(string: String): InferenceRule.Value = string match {
    case "DNE" => InferenceRule.DNE
    case "DNI" => InferenceRule.DNI
    case "CE" => InferenceRule.CE
    case "CI" => InferenceRule.CI
    case "MT" => InferenceRule.MT
    case _ => throw new PendingException()
  }

  Given("""^a deduction exists with premises "([^"]*)"$""") { (arg0: String) =>
    premises = arg0.split(",").toSeq.map(_.trim).flatMap(f => Formula.read(f))
  }

  When("""^I attempt to prove "([^"]*)"$""") { (arg0: String) =>
    conclusion = Formula.read(arg0).get
    deduction = new Deduction(conclusion, premises) with InferenceHard
    Await.result(Future (deduction.prove(conclusion)), timeoutInSeconds seconds) match {
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

  Then("""^the deduction fails""") { () =>
    conclusionNode shouldBe None
  }

  Then("""^show me the deduction$""") { () =>
    println(deduction.write)
  }

  Then("""^the deduction is (\d+) lines long$""") { (length: Int) =>
    deductionSequence.length shouldBe length
  }

  Then("""^the (\d+)(?:nd|st|rd|th) line in the deduction is a ([A-Z]{2,3})$""") { (line: Int, inferenceRuleString: String) =>
    deductionSequence.nodes(line - 1).inferenceRule shouldBe stringToInferenceRule(inferenceRuleString)
  }


}
