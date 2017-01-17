package com.gray.logic.deduction.inference

import com.gray.logic.deduction._
import com.gray.logic.formula._
import com.gray.logic.operation.Extraction
import com.gray.logic.tools.Logging

trait InferenceHard extends InferenceSoft with Extraction with Logging {

  override def infer(request: DeductionRequest): DeductionResult = inferHard(request)

  def inferHard(request: DeductionRequest): DeductionResult = super.infer(request) match {
    case success: DeductionSuccess => success
    case DeductionFailure =>
      doFirstSuccessful2(request)(Seq(
        inferCP, inferMP
      ))
  }

  def inferMP(request: DeductionRequest) = request.stack.check(request.conclusion, InferenceRule.MP) { newStack =>
    logger.info(s"INFERRING MP HARD [${request.conclusion.write}]")
    val conditionals = allFormulaDecompositions(request.sequence.getDistinctFormulas).filter {
      case Conditional(ant, cons) => cons == request.conclusion
      case _ => false
    }.asInstanceOf[List[Conditional]]

    var result: DeductionResult = DeductionFailure

    conditionals.find { conditional =>
      inferHard(DeductionRequest(conditional, request.sequence, newStack)) match {
        case DeductionFailure => false
        case DeductionSuccess(conditionalNode, conditionalSeq, conditionalStack) =>
          val antecedent = conditional.left
          inferHard(DeductionRequest(antecedent, conditionalSeq, conditionalStack)) match {
            case DeductionFailure => false
            case DeductionSuccess(antecedentNode, antecedentSequence, concStack) =>
              val (node, sequence) = antecedentSequence.addMP(request.conclusion, conditionalNode, antecedentNode)
              result = DeductionSuccess(node, sequence, concStack)
              true
          }
      }
    }
    result
  }

  def inferCP(request: DeductionRequest) = continueWithFormula[Conditional](request.conclusion) { conditional =>
    logger.info(s"INFERRING CP HARD: [${request.conclusion.write}]")
    request.stack.check(request.conclusion, InferenceRule.CP) { newStack =>
      val antecedent = conditional.left
      val consequent = conditional.right

      val (assumptionNode, assumptionSequence) = request.sequence.addAssumptionCP(antecedent)
      inferHard(DeductionRequest(consequent, assumptionSequence, newStack)) match {
        case DeductionFailure => DeductionFailure
        case DeductionSuccess(consequentNode, consequentSequence, concStack) =>
          val (concNode, concSequence) = consequentSequence.addCP(request.conclusion, assumptionNode, consequentNode)
          DeductionSuccess(concNode, concSequence, concStack)
      }
    }
  }

  def inferDE_Hard(conclusion: Formula) = ???

}
