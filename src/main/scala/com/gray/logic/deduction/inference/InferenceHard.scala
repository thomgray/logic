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
        inferCP, inferMP,
        inferDE_Hard

      ))
  }

  def inferMP(request: DeductionRequest) = request.stack.check(request.conclusion, InferenceRule.MP) { newStack =>
    logger.info(s"Infer MP Hard [${request.conclusion.write}]")
    val conditionals = allFormulaDecompositions(request.sequence.getDistinctFormulas) collect {
      case conditional@Conditional(ant, cons) if cons == request.conclusion => conditional
    }

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
    info(s"Infer CP Hard: [${request.conclusion.write}]")
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

  def inferDE_Hard(request: DeductionRequest) = request.stack.check(request.conclusion, InferenceRule.DE) { stack1 =>
    info(s"infer DE Hard: ${request.conclusion.write}")
    val (conclusion, sequence1, _) = request.decompose
    val disjunctions = allFormulaDecompositions(sequence1.getDistinctFormulas).collect {
      case dj: Disjunction => dj
    }

    var result: DeductionResult = DeductionFailure

    disjunctions find { disjunction =>
      val (leftDj, rightDj) = (disjunction.left, disjunction.right)
      stack1.checkDisjunction(disjunction) { stack2 =>
        val stack3 = stack2.addRestriction(disjunction, InferenceRule.DI)
        inferHard(DeductionRequest(disjunction, sequence1, stack3)) match {
          case DeductionSuccess(disjunctionNode, djSequence, djStack) =>
            info(s"Got a disjunction [${disjunction.write}]")
            inferDEPart(leftDj, conclusion, djSequence, djStack) match {
              case None => DeductionFailure
              case Some((leftDjNode, minorConc1Node, sequence3, stack3)) =>
                info(s"Proved [${leftDj.write} -> ${conclusion.write}]")
                inferDEPart(rightDj, conclusion, sequence3.stepDown, stack3) match {
                  case None => DeductionFailure
                  case Some((rightDjNode, minorConc2Node, sequence4, stack4)) =>
                    info(s"Proved [${rightDj.write} -> ${conclusion.write}, DE successful]")
                    val (concNode, concSequence) = sequence4.addDE(conclusion, disjunctionNode, leftDjNode, minorConc1Node, rightDjNode, minorConc2Node)
                    DeductionSuccess(concNode, concSequence, stack4)
                }
            }
          case DeductionFailure => DeductionFailure
        }
      } match {
        case success: DeductionSuccess =>
          result = success
          true
        case DeductionFailure => false
      }
    }
    result
  }

  private def inferDEPart(disjunct: Formula, minorConc: Formula, sequence: DeductionSequence, stack: DeductionStack) = {
    val (assumptionNode, sequence2) = sequence.addAssumptionDE(disjunct)
    inferHard(DeductionRequest(minorConc, sequence2, stack)) match {
      case DeductionSuccess(minorConcNode, sequence3, stack2) =>
        Some(assumptionNode, minorConcNode, sequence3, stack2)
      case _ => None
    }
  }

}
