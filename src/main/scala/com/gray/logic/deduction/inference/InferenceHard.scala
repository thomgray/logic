package com.gray.logic.deduction.inference

import com.gray.logic.deduction._
import com.gray.logic.formula._
import com.gray.logic.tools.Logging

trait InferenceHard extends InferenceSoft with InferenceUtils with Decision with Logging {

  override def infer(request: DeductionRequest): Result = inferHard(request)

  def inferHard(request: DeductionRequest): Result = super.infer(request) match {
    case success: Proven => success
    case Unproven =>
      logger.info(s"Infer Hard [${request.conclusion.write}]")
      doFirstSuccessful(request)(Seq(
        inferCI_Hard, inferCE_Hard,
        inferCP_Hard, inferMP_Hard, inferMT_Hard,
        inferDE_Hard, inferDI_Hard,
        inferDNE_Hard, inferDNI_Hard,
        inferRAA_Hard
      ))
  }

  def inferAny(requests: DeductionRequest*): Result = {
    requests foreach (req => inferHard(req) match {
      case proven: Proven => return proven
      case _ =>
    })
    Unproven
//    firstSuccess(requests.map { request =>
//      () => inferHard(request)
//    })
  }

  def inferCI_Hard(request: DeductionRequest) = continueWith[Conjunction](request.conclusion) { conjunction =>
    request.check(InferenceRule.CI) { stack1 =>
      val stack11 = stack1.addRestriction(conjunction.left, InferenceRule.CE)
      inferHard(DeductionRequest(conjunction.left, request.sequence, stack11)) { (leftNode, seq2, _) =>
        val stack12 = stack1.addRestriction(conjunction.right, InferenceRule.CE)
        inferHard(DeductionRequest(conjunction.right, seq2, stack12)) { (rightNode, seq3, _) =>
          val (concNode, concSeq) = seq3.addCI(conjunction, leftNode, rightNode)
          Proven(concNode, concSeq, stack1)
        }
      }
    }
  }

  def inferCE_Hard(request: DeductionRequest) = request.check(InferenceRule.CE) { stack1 =>
    val conjunctions = getConjunctionsForCE(request.sequence, request.conclusion)
    val ops = conjunctions map { conjunction => () =>
      val stack11 = stack1.addRestriction(conjunction, InferenceRule.CI)
      inferHard(DeductionRequest(conjunction, request.sequence, stack11))
    }

    doFirstSuccessful(ops) { (conjunctNode, conjunctSeq, _) =>
      inferCI_Soft(DeductionRequest(request.conclusion, conjunctSeq, stack1))
    }
  }

  def inferMP_Hard(request: DeductionRequest) = request.check(InferenceRule.MP) { newStack =>
    logger.info(s"Infer MP Hard [${request.conclusion.write}]")
    val conditionals = getConditionals(request.sequence, request.conclusion == _.right)

    var result: Result = Unproven

    val ops = conditionals map { conditional => () =>
      val requestStack = newStack.addRestriction(conditional, InferenceRule.CP)
      inferHard(DeductionRequest(conditional, request.sequence, requestStack)) { conditionalResult =>
        val antecedent = conditional.left
        inferHard(DeductionRequest(antecedent, conditionalResult.sequence, conditionalResult.stack)) { antResult =>
          val (antecedentNode, antecedentSequenece, concStack) = Proven.unapply(antResult).get
          val (node, sequence) = antecedentSequenece.addMP(request.conclusion, conditionalResult.deductionNode, antecedentNode)
          Proven(node, sequence, concStack)
        }
      }
    }
    doFirstSuccessful(ops)
  }

  def inferCP_Hard(request: DeductionRequest) = continueWith[Conditional](request.conclusion) { conditional =>
    info(s"Infer CP Hard: [${request.conclusion.write}]")
    request.check(InferenceRule.CP) { newStack =>
      val antecedent = conditional.left
      val consequent = conditional.right

      val (assumptionNode, assumptionSequence) = request.sequence.addAssumptionCP(antecedent)
      inferHard(DeductionRequest(consequent, assumptionSequence, newStack)) { proven =>
        val (consequentNode, consequentSequence, concStack) = Proven.unapply(proven).get
        val (concNode, concSequence) = consequentSequence.addCP(request.conclusion, assumptionNode, consequentNode)
        Proven(concNode, concSequence, concStack)
      }
    }
  }

  def inferDI_Hard(request: DeductionRequest) = continueWith[Disjunction](request.conclusion) { disjunction =>
    info(s"Infer DI Hard: [${request.conclusion.write}]")
    request.check(InferenceRule.DI) { stack1 =>
      inferAny(
        DeductionRequest(disjunction.left, request.sequence, stack1),
        DeductionRequest(disjunction.right, request.sequence, stack1)
      ) match {
        case Unproven => Unproven
        case Proven(djNode, djSequence, djStack) =>
          val (concNode, concSeq) = djSequence.addDI(disjunction, djNode)
          Proven(concNode, concSeq, djStack)
      }
    }
  }

  def inferDE_Hard(request: DeductionRequest) = request.check(InferenceRule.DE) { stack1 =>
    info(s"infer DE Hard: ${request.conclusion.write}")
    val (conclusion, sequence1, _) = request.decompose
    val disjunctions = getDisjunctions(sequence1)

    doFirstSuccessful {
      disjunctions map { disjunction => () =>
        val (leftDj, rightDj) = (disjunction.left, disjunction.right)
        stack1.checkDisjunction(disjunction) { stack2 =>
          val stack3 = stack2.addRestriction(disjunction, InferenceRule.DI)
          inferHard(DeductionRequest(disjunction, sequence1, stack3)) { proven1: Proven =>
            val (disjunctionNode, djSequence, djStack) = Proven.unapply(proven1).get
            info(s"Got a disjunction [${disjunction.write}]")
            inferDEPart(leftDj, conclusion, djSequence, djStack) match {
              case None => Unproven
              case Some((leftDjNode, minorConc1Node, sequence3, stack4)) =>
                info(s"Proved [${leftDj.write} -> ${conclusion.write}]")
                inferDEPart(rightDj, conclusion, sequence3.stepDown, stack4) match {
                  case None => Unproven
                  case Some((rightDjNode, minorConc2Node, sequence4, stack5)) =>
                    info(s"Proved [${rightDj.write} -> ${conclusion.write}, DE successful]")
                    val (concNode, concSequence) = sequence4.addDE(conclusion, disjunctionNode, leftDjNode, minorConc1Node, rightDjNode, minorConc2Node)
                    Proven(concNode, concSequence, stack5)
                }
            }
          }
        }
      }
    }
  }

  private def inferDEPart(disjunct: Formula, minorConc: Formula, sequence: DeductionSequence, stack: DeductionStack) = {
    val (assumptionNode, sequence2) = sequence.addAssumptionDE(disjunct)
    inferHard(DeductionRequest(minorConc, sequence2, stack)) match {
      case Proven(minorConcNode, sequence3, stack2) =>
        Some(assumptionNode, minorConcNode, sequence3, stack2)
      case _ => None
    }
  }

  def inferDNE_Hard(request: DeductionRequest) = {
    worthDoingDNE(request.conclusion, request.sequence) { () =>
      request.check(InferenceRule.DNE) { stack1 =>
        info(s"Infer DNE: [${request.conclusion.write}]")
        val dni = Negation(Negation(request.conclusion))
        val stack11 = stack1.addRestriction(dni, InferenceRule.DNI)
        inferHard(DeductionRequest(dni, request.sequence, stack11)) { (dniNode, dniSeq, dniStack) =>
          val (concNode, concSequence) = dniSeq.addDNE(request.conclusion, dniNode)
          Proven(concNode, concSequence, dniStack)
        }
      }
    }
  }

  def inferDNI_Hard(request: DeductionRequest) = continueWith[Negation](request.conclusion) { negation1 =>
    continueWith[Negation](negation1.formula) { negation2 =>
      request.check(InferenceRule.DNI) { stack1 =>
        info(s"Infer DE Part: [${request.conclusion.write}]")
        val stack11 = stack1.addRestriction(negation2.formula, InferenceRule.DNE)
        inferHard(DeductionRequest(negation2.formula, request.sequence, stack11)) { proven =>
          val (dneNode, dneSeq, dneStack) = Proven.unapply(proven).get
          val (concNode, concSeq) = dneSeq.addDNI(negation1, dneNode)
          Proven(concNode, concSeq, dneStack)
        }
      }
    }
  }

  def inferMT_Hard(request: DeductionRequest) = continueWith[Negation](request.conclusion) { negation =>
    val sequence1 = request.sequence
    request.check(InferenceRule.MT) { stack1 =>
      info(s"Infer MT Hard: [${request.conclusion.write}]")
      val conditionals = getConditionals(sequence1, _.left == negation.formula)
      var result: Result = Unproven
      conditionals.find { conditional =>
        inferHard(DeductionRequest(conditional, sequence1, stack1)) match {
          case Unproven => false
          case Proven(conditionalNode, sequence2, stack2) =>
            val negatedConsequent = Negation(conditional.right)
            inferHard(DeductionRequest(negatedConsequent, sequence2, stack1)) match {
              case Unproven => false
              case Proven(negConsNode, sequence3, stack3) =>
                val (concNode, concSeq) = sequence3.addMT(negation, conditionalNode, negConsNode)
                result = Proven(concNode, concSeq, stack3)
                true
            }
        }
      }
      result
    }
  }


  def inferRAA_Hard(request: DeductionRequest) = continueWith[Negation](request.conclusion) { negationConclusion =>
    request.check(InferenceRule.RAA) { stack1 =>
      logger.info(s"Infer RAA Hard ${request.conclusion.write}")
      val (assumptionNode, sequence1) = request.sequence.addAssumptionRAA(negationConclusion.formula)
      val formulasForRaa = getFormulasForRAA(sequence1)

      doFirstSuccessful {
        formulasForRaa map { formula => () =>
          val negation = Negation(formula)
          inferHard(DeductionRequest(formula, sequence1, stack1)) { (n2, s2, stack2) =>
            inferHard(DeductionRequest(negation, s2, stack1)) { (n3, s3, _) =>
              val (concNode, concseq) = s3.addRAA(negationConclusion, assumptionNode, n2, n3)
              Proven(concNode, concseq, stack1)
            }
          }
        }
      }
    }
  }
}
