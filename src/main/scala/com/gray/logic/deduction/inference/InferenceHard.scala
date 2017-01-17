package com.gray.logic.deduction.inference

import com.gray.logic.deduction._
import com.gray.logic.formula.{Conditional, Disjunction, Formula}
import com.gray.logic.operation.Extraction

trait InferenceHard extends InferenceSoft with Extraction {

  def infer2(request: DeductionRequest): DeductionResult = doFirstSuccessful2(request)(Seq(
    inferCP, inferMP
  ))

  def inferMP(request: DeductionRequest) = request.stack.check(request.conclusion, InferenceRule.MP) { newStack =>
    val conditionals = allFormulaDecompositions(request.sequence.getDistinctFormulas).filter {
      case Conditional(ant, cons) => cons == request.conclusion
      case _ => false
    }.asInstanceOf[List[Conditional]]

    var result: DeductionResult = DeductionFailure

    conditionals.find{ conditional =>
      infer2(DeductionRequest(conditional, request.sequence, newStack)) match {
        case DeductionFailure => false
        case DeductionSuccess(conditionalNode, conditionalSeq, conditionalStack) =>
          val antecedent = conditional.left
          infer2(DeductionRequest(antecedent, conditionalSeq, conditionalStack)) match {
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
    request.stack.check(request.conclusion, InferenceRule.CP) { newStack =>
      val antecedent = conditional.left
      val consequent = conditional.right

      val (assumptionNode, assumptionSequence) = request.sequence.addAssumptionCP(antecedent)
      infer2(DeductionRequest(consequent, assumptionSequence, newStack)) match {
        case DeductionFailure => DeductionFailure
        case DeductionSuccess(consequentNode, consequentSequence, concStack) =>
          val (concNode, concSequence) = consequentSequence.addCP(request.conclusion, assumptionNode, consequentNode)
          DeductionSuccess(concNode, concSequence, concStack)
      }
    }
  }

  def inferDE_Hard(conclusion: Formula) = ???

}
