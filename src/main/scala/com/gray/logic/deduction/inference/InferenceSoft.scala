package com.gray.logic.deduction.inference

import com.gray.logic.deduction._
import com.gray.logic.formula._
import com.gray.logic.language.FormulaWriterAlphabetic
import com.gray.logic.mixin.ControlFlow

trait InferenceSoft extends Inference {

  override def infer(request: DeductionRequest) = inferSoft(request)

  private def inferSoft(request: DeductionRequest): Result = {
    request.sequence.findInDeduction(request.conclusion) match {
      case Some((node, sequence)) => Proven(node, sequence)
      case _ =>
        doFirstSuccessful(request)(Seq(
          inferCI_Soft, inferCE_Soft,
          inferBI_Soft, inferBE_Soft,
          inferDNI_Soft, inferDNE_Soft,
          inferMP_Soft, inferMT_Soft,
          inferCP_Soft,
          inferDI_Soft,
          inferRAA_Soft
        ))
    }
  }

  def inferMP_Soft(request: DeductionRequest): Result = request.check(InferenceRule.MP) { _ =>
    val conditionals = request.sequence.findAllInDeduction(node => node.formula match {
      case Conditional(ant, cons) => cons == request.conclusion
      case _ => false
    })

    var result: Result = Unproven

    conditionals.find { tuple =>
      val (conditionalNode, conditionalSequence) = tuple
      val antecedent = conditionalNode.formula.asInstanceOf[Conditional].left

      conditionalSequence.findInDeduction(antecedent) match {
        case Some((antecedentNode, antecedentSequence)) =>
          result = antecedentSequence.addMP(request.conclusion, conditionalNode, antecedentNode)
          true
        case _ => false
      }
    }
    result
  }


  def inferMT_Soft(request: DeductionRequest): Result = continueWith[Negation](request.conclusion) { negation =>
    request.check(InferenceRule.MT) { _ =>
      val (conclusion, sequence, stack) = request.decompose
      val conditionals = sequence.findAllInDeduction(node => node match {
        case DeductionNode(Conditional(ant, cons), _) => ant == negation.formula
        case _ => false
      })
      var result: Result = Unproven
      conditionals.find { tuple =>
        val (conditionalNode, conditionalSeq) = (tuple._1, tuple._2)
        val conditionalFormula = conditionalNode.formula.asInstanceOf[Conditional]
        conditionalSeq.findInDeduction(Negation(conditionalFormula.right)) match {
          case None => false
          case Some((negConsNode, negConsSeq)) =>
            result =  negConsSeq.addMT(conclusion, conditionalNode, negConsNode)
            true
        }
      }
      result
    }
  }


  def inferCI_Soft(request: DeductionRequest): Result = continueWith[Conjunction](request.conclusion) { conjunction =>
    request.check(InferenceRule.CI) { stack =>
      val (left, right) = (conjunction.left, conjunction.right)
      inferSoft(DeductionRequest(left, request.sequence, request.stack)) match {
        case Unproven => Unproven
        case Proven(leftNode, leftSeq) =>
          inferSoft(DeductionRequest(right, leftSeq, stack)) match {
            case Unproven => Unproven
            case Proven(rightNode, rightSeq) =>
              rightSeq.addCI(request.conclusion, leftNode, rightNode)
          }
      }
    }
  }

  def inferCE_Soft(request: DeductionRequest) = request.check(InferenceRule.CE) { _ =>
    val (conclusion, sequence, stack) = request.decompose
    sequence.findInDeduction(f => f match {
      case DeductionNode(Conjunction(left, right), _) => left == conclusion || right == conclusion
      case _ => false
    }) match {
      case Some((conjunctionNode, conjunctionSequence)) =>
        conjunctionSequence.addCE(conclusion, conjunctionNode)
      case _ => Unproven
    }
  }

  def inferDI_Soft(request: DeductionRequest): Result = request.check(InferenceRule.DI) { _ =>
    val (conclusion, sequence, stack) = request.decompose
    continueWith[Disjunction](conclusion) { disjunction =>
      val (dj1, dj2) = (disjunction.left, disjunction.right)
      inferSoft(DeductionRequest(dj1, sequence, stack)) match {
        case Proven(dj1Node, dj1Seq) =>
          dj1Seq.addDI(conclusion, dj1Node)
        case Unproven =>
          inferSoft(DeductionRequest(dj2, sequence, stack)) match {
            case Proven(dj2Node, dj2Seq) =>
              dj2Seq.addDI(conclusion, dj2Node)
            case _ => Unproven
          }
      }
    }
  }

  def inferDE_Soft(request: DeductionRequest): Result = Unproven

  def inferBI_Soft(request: DeductionRequest) = request.check(InferenceRule.BI) { _ =>
    val (conclusion, sequence, stack) = request.decompose
    continueWith[Biconditional](conclusion) { biconditional =>
      val conditional1 = Conditional(biconditional.left, biconditional.right)
      val conditional2 = Conditional(biconditional.right, biconditional.left)

      inferSoft(DeductionRequest(conditional1, sequence, stack)) match {
        case Proven(conditional1Node, conditional1Seq) =>
          inferSoft(DeductionRequest(conditional2, conditional1Seq, stack)) match {
            case Proven(conditional2Node, conditional2Seq) =>
              conditional2Seq.addBI(conclusion, conditional1Node, conditional2Node)
            case _ => Unproven
          }
        case _ => Unproven
      }
    }
  }

  def inferBE_Soft(request: DeductionRequest): Result = continueWith[Conditional](request.conclusion) { cond =>
    request.check(InferenceRule.BE) { _ =>
      request.sequence.findInDeduction(f => f match {
        case DeductionNode(Biconditional(left, right), _) => (left == cond.left && right == cond.right) || (right == cond.left && left == cond.right)
        case _ => false
      }) match {
        case Some((biconNode, biconSequence)) =>
          biconSequence.addBE(request.conclusion, biconNode)
        case _ => Unproven
      }
    }
  }

  def inferCP_Soft(request: DeductionRequest): Result = continueWith[Conditional](request.conclusion) { conditional =>
    request.check(InferenceRule.CP) { _ =>
      val antecedent = conditional.left
      val consequent = conditional.right
      val (assumption, assumptionSeq) = request.sequence.addAssumptionCP(antecedent).decompose
      inferSoft(DeductionRequest(consequent, assumptionSeq, request.stack)) match {
        case Unproven => Unproven
        case Proven(consequentNode, consequenceSeq) =>
          consequenceSeq.addCP(request.conclusion, assumption, consequentNode)
      }
    }
  }

  def inferDNI_Soft(request: DeductionRequest) = continueWith[Negation](request.conclusion) { neg1 =>
    continueWith[Negation](neg1.formula) { negation2 =>
      request.check(InferenceRule.DNI) { _ =>
        val (conclusion, sequence, stack) = request.decompose
        inferSoft(DeductionRequest(negation2.formula, sequence, stack)) match {
          case Proven(dneNode, dneSeq) =>
            dneSeq.addDNI(conclusion, dneNode)
          case Unproven => Unproven
        }
      }
    }
  }

  def inferDNE_Soft(request: DeductionRequest) = request.check(InferenceRule.DNE) { _ =>
    val (conclusion, sequence, stack) = request.decompose
    val formulaToFind = Negation(Negation(conclusion))
    sequence.findInDeduction(formulaToFind) match {
      case Some((node, sequence2)) =>
        sequence2.addDNE(conclusion, node)
      case _ => Unproven
    }
  }

  def inferRAA_Soft(request: DeductionRequest) = request.check(InferenceRule.RAA) { _ =>
    val (conclusion, sequence, stack) = request.decompose
    val nonNegated = sequence.findAllInDeduction(n => !n.formula.isInstanceOf[Negation])

//    nonNegated find { tuple =>
//      val (node, seq) = tuple
//      seq.findInDeduction(node.formula) match {
//        case Some(node1, seq1) =>
//      }
//    }
    
    Unproven
  }

}
