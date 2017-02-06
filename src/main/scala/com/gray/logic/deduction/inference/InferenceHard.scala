package com.gray.logic.deduction.inference

import com.gray.logic.deduction._
import com.gray.logic.formula._
import com.gray.logic.tools.Logging

trait InferenceHard extends InferenceSoft with InferenceUtils with Decision with Logging {

  override def infer(request: DeductionRequest): Result = inferWithRAA(request)

  def inferWithRAA(request: DeductionRequest) =
    inferHard(request) match {
      case Unproven => inferRAA_Hard(request)
      case other => other
    }

  def inferHard(conclusion: Formula, sequence: DeductionSequence, stack: DeductionStack): Result = inferHard(DeductionRequest(conclusion, sequence, stack))

  def inferHard(request: DeductionRequest): Result = super.infer(request) match {
    case success: Proven => success
    case Unproven =>
      logger.info(s"Infer Hard [${request.conclusion.write}]")
      doFirstSuccessful(request)(Seq(
        inferCI_Hard, inferCE_Hard,
        inferCP_Hard, inferMP_Hard, inferMT_Hard,
        inferDE_Hard, inferDI_Hard,
        inferDNE_Hard, inferDNI_Hard, inferRAA_Hard
      ))
  }

  def inferAny(requests: DeductionRequest*): Result = {
    requests foreach (req => inferHard(req) match {
      case proven: Proven => return proven
      case _ =>
    })
    Unproven
  }

  def inferCI_Hard(request: DeductionRequest) = continueWith[Conjunction](request.conclusion) { conjunction =>
    request.check(InferenceRule.CI) { stack1 =>
      val stack11 = stack1.addRestriction(conjunction.left, InferenceRule.CE)
      inferHard(conjunction.left, request.sequence, stack11) { (leftNode, seq2) =>
        val stack12 = stack1.addRestriction(conjunction.right, InferenceRule.CE)
        inferHard(conjunction.right, seq2, stack12) { (rightNode, seq3) =>
          seq3.addCI(conjunction, leftNode, rightNode)
        }
      }
    }
  }

  def inferCE_Hard(request: DeductionRequest) = request.check(InferenceRule.CE) { stack1 =>
    val conjunctions = getConjunctionsForCE(request.sequence, request.conclusion)
    val ops = conjunctions map { conjunction => () =>
      val stack11 = stack1.addRestriction(conjunction, InferenceRule.CI)
      inferHard(conjunction, request.sequence, stack11)
    }
    doFirstSuccessful(ops) { (conjunctNode, conjunctSeq) =>
      conjunctSeq.addCE(request.conclusion, conjunctNode)
    }
  }

  def inferMP_Hard(request: DeductionRequest) = request.check(InferenceRule.MP) { newStack =>
    logger.info(s"Infer MP Hard [${request.conclusion.write}]")
    val conditionals = getConditionals(request.sequence, request.conclusion == _.right)

    var result: Result = Unproven

    val ops = conditionals map { conditional => () =>
      val requestStack = newStack.addRestriction(conditional, InferenceRule.CP)
      inferHard(conditional, request.sequence, requestStack) { conditionalResult =>
        val antecedent = conditional.left
        inferHard(antecedent, conditionalResult.sequence, newStack) { (antecedentNode, antecedentSequenece) =>
          antecedentSequenece.addMP(request.conclusion, conditionalResult.deductionNode, antecedentNode)
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

      val (assumptionNode, assumptionSequence) = request.sequence.addAssumptionCP(antecedent).decompose
      inferHard(consequent, assumptionSequence, newStack) { (consequentNode, consequentSequence) =>
        consequentSequence.addCP(request.conclusion, assumptionNode, consequentNode)
      }
    }
  }

  def inferDI_Hard(request: DeductionRequest) = continueWith[Disjunction](request.conclusion) { disjunction =>
    request.check(InferenceRule.DI) { stack1 =>
      info(s"Infer DI Hard: [${request.conclusion.write}]")
      inferAny(
        DeductionRequest(disjunction.left, request.sequence, stack1),
        DeductionRequest(disjunction.right, request.sequence, stack1)
      ) match {
        case Unproven => Unproven
        case Proven(djNode, djSequence) =>
          djSequence.addDI(disjunction, djNode)
      }
    }
  }

  def inferDE_Hard(request: DeductionRequest) = request.check(InferenceRule.DE) { stack1 =>
    val (conclusion, sequence1, _) = request.decompose
    val disjunctions = getDisjunctions(sequence1)

    val ops = disjunctions map { disjunction => () =>
      info(s"Infer DE Hard: [${request.conclusion.write}] with disjunction [${disjunction.write}]")
      val (leftDj, rightDj) = (disjunction.left, disjunction.right)
      val stack2 = stack1.addRestriction(disjunction, InferenceRule.DI)
      stack2.checkDisjunction(disjunction) { stack3 =>
        inferHard(disjunction, sequence1, stack3) { (disjunctionNode, djSequence) =>
          info(s"Got a disjunction [${disjunction.write}] by ${disjunctionNode.inferenceRule}")
          inferDEPart(leftDj, conclusion, djSequence, stack3) match {
            case None => Unproven
            case Some((leftDjNode, minorConc1Node, sequence3)) =>
              info(s"Proved [${leftDj.write} -> ${conclusion.write}]")
              inferDEPart(rightDj, conclusion, sequence3.stepDown, stack3) match {
                case None => Unproven
                case Some((rightDjNode, minorConc2Node, sequence4)) =>
                  info(s"Proved [${rightDj.write} -> ${conclusion.write}, DE successful]")
                  sequence4.addDE(conclusion, disjunctionNode, leftDjNode, minorConc1Node, rightDjNode, minorConc2Node)
              }
          }
        }
      }
    }
    doFirstSuccessful(ops)
  }

  private def inferDEPart(disjunct: Formula, minorConc: Formula, sequence: DeductionSequence, stack: DeductionStack) = {
    val (assumptionNode, sequence2) = sequence.addAssumptionDE(disjunct).decompose
    inferHard(minorConc, sequence2, stack) match {
      case Proven(minorConcNode, sequence3) =>
        Some(assumptionNode, minorConcNode, sequence3)
      case _ => None
    }
  }

  def inferDNE_Hard(request: DeductionRequest) = {
    worthDoingDNE(request.conclusion, request.sequence) { () =>
      request.check(InferenceRule.DNE) { stack1 =>
        info(s"Infer DNE: [${request.conclusion.write}]")
        val dni = Negation(Negation(request.conclusion))
        val stack11 = stack1.addRestriction(dni, InferenceRule.DNI)
        inferHard(dni, request.sequence, stack11) { (dniNode, dniSeq) =>
          dniSeq.addDNE(request.conclusion, dniNode)
        }
      }
    }
  }

  def inferDNI_Hard(request: DeductionRequest) = continueWith[Negation](request.conclusion) { negation1 =>
    continueWith[Negation](negation1.formula) { negation2 =>
      request.check(InferenceRule.DNI) { stack1 =>
        info(s"Infer DE Part: [${request.conclusion.write}]")
        val stack11 = stack1.addRestriction(negation2.formula, InferenceRule.DNE)
        inferHard(negation2.formula, request.sequence, stack11) { (dneNode, dneSeq) =>
          dneSeq.addDNI(negation1, dneNode)
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

      val ops = conditionals map (conditional => () => {
        inferHard(conditional, sequence1, stack1) { (conditionalNode, conditionalSeq) =>
          val negatedConsequent = Negation(conditional.right)
          inferHard(negatedConsequent, conditionalSeq, stack1) { (negConsNode, negConsSeq) =>
            negConsSeq.addMT(negation, conditionalNode, negConsNode)
          }
        }
      })

      doFirstSuccessful(ops)
    }
  }


  def inferRAA_Hard(request: DeductionRequest) = continueWith[Negation](request.conclusion) { negationConclusion =>
    request.check(InferenceRule.RAA) { stack1 =>
      logger.info(s"Infer RAA Hard ${request.conclusion.write}")
      val (assumptionNode, sequence1) = request.sequence.addAssumptionRAA(negationConclusion.formula).decompose
      val formulasForRaa = getFormulasForRAA(sequence1)

      val ops = formulasForRaa map (formula => () => {
        val negation = Negation(formula)
        inferHard(formula, sequence1, stack1) { (n2, s2) =>
          inferHard(negation, s2, stack1) { (n3, s3) =>
            s3.addRAA(negationConclusion, assumptionNode, n2, n3)
          }
        }
      })
      doFirstSuccessful(ops)
    }
  }


}
