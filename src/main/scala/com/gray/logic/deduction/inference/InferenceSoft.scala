package com.gray.logic.deduction.inference

import com.gray.logic.deduction._
import com.gray.logic.formula._
import com.gray.logic.language.FormulaWriterAlphabetic
import com.gray.logic.mixin.ControlFlow

trait InferenceSoft extends Inference with ControlFlow {

  override def infer(request: DeductionRequest) = inferSoft(request)

  private def inferSoft(request: DeductionRequest): DeductionResult = {
    request.sequence.findInDeduction(request.conclusion) match {
      case Some((node, sequence)) => DeductionSuccess(node, sequence, request.stack)
      case _ =>
        doFirstSuccessful2(request)(Seq(
          inferCI_Soft, inferCE_Soft,
          inferBI_Soft, inferBE_Soft,
          inferDNI_Soft, inferDNE_Soft,
          inferMP_Soft, inferMT_Soft,
          inferCP_Soft,
          inferDI_Soft
        ))
    }
  }

  def inferMP_Soft(deductionRequest: DeductionRequest): DeductionResult = {
    val conditionals = deductionRequest.sequence.findAllInDeduction(node => node.formula match {
      case Conditional(ant, cons)  => cons==deductionRequest.conclusion
      case _ => false
    })

    var result : DeductionResult = DeductionFailure

    conditionals.find { tuple =>
      val (conditionalNode, conditionalSequence) = tuple
      val antecedent = conditionalNode.formula.asInstanceOf[Conditional].left

      conditionalSequence.findInDeduction(antecedent) match {
        case Some((antecedentNode, antecedentSequence)) =>
          val (concNode, concSequence) = antecedentSequence.addMP(deductionRequest.conclusion, conditionalNode, antecedentNode)
          result = DeductionSuccess(concNode, concSequence, deductionRequest.stack)
          true
        case _ => false
      }
    }
    result
  }


  def inferMT_Soft(request: DeductionRequest): DeductionResult = DeductionFailure


  def inferCI_Soft(request: DeductionRequest): DeductionResult = continueWithFormula[Conjunction](request.conclusion){ conjunction =>
    val (left, right) = (conjunction.left, conjunction.right)
    inferSoft(DeductionRequest(left, request.sequence, request.stack)) match {
      case DeductionFailure => DeductionFailure
      case DeductionSuccess(leftNode, leftSeq, leftStack) =>
        inferSoft(DeductionRequest(right, leftSeq, leftStack)) match {
          case DeductionFailure => DeductionFailure
          case DeductionSuccess(rightNode, rightSeq, rightStack) =>
            val (concNode, concSeq) = rightSeq.addCI(request.conclusion, leftNode, rightNode)
            DeductionSuccess(concNode, concSeq, rightStack)
        }
    }
  }

  def inferCE_Soft(request: DeductionRequest) = {
    val (conclusion, sequence, stack) = request.decompose
    sequence.findInDeduction(f => f match {
      case DeductionNode(Conjunction(left, right), _) => left == conclusion || right == conclusion
      case _ => false
    }) match {
      case Some((conjunctionNode, conjunctionSequence)) =>
        val (concNode, concSeq) = conjunctionSequence.addCE(conclusion, conjunctionNode)
        DeductionSuccess(concNode, concSeq, stack)
      case _ => DeductionFailure
    }
  }

  def inferDI_Soft(request: DeductionRequest): DeductionResult = {
    val (conclusion, sequence, stack) = request.decompose
    continueWithFormula[Disjunction](conclusion){ disjunction =>
      val (dj1, dj2) = (disjunction.left, disjunction.right)
      inferSoft(DeductionRequest(dj1, sequence, stack)) match {
        case DeductionSuccess(dj1Node, dj1Seq, dj1Stack) =>
          val (concNode, concSeq) = dj1Seq.addDI(conclusion, dj1Node)
          DeductionSuccess(concNode, concSeq, stack)
        case DeductionFailure =>
          inferSoft(DeductionRequest(dj2,sequence, stack)) match {
            case DeductionSuccess(dj2Node, dj2Seq, dj2Stack) =>
              val (concNode, concSeq) = dj2Seq.addDI(conclusion, dj2Node)
              DeductionSuccess(concNode, concSeq, stack)
            case _ => DeductionFailure
          }
      }
    }
  }

  def inferDE_Soft(request: DeductionRequest): DeductionResult = DeductionFailure

  def inferBI_Soft(request: DeductionRequest) = {
    val (conclusion, sequence, stack) = request.decompose
    continueWithFormula[Biconditional](conclusion) { biconditional =>
      val conditional1 = Conditional(biconditional.left, biconditional.right)
      val conditional2 = Conditional(biconditional.right, biconditional.left)

      inferSoft(DeductionRequest(conditional1, sequence, stack)) match {
        case DeductionSuccess(conditional1Node, conditional1Seq, conditional1Stack) =>
          inferSoft(DeductionRequest(conditional2, conditional1Seq, conditional1Stack)) match {
            case DeductionSuccess(conditional2Node, conditional2Seq, conditional2Stack) =>
              val (concNode, concSeq) = conditional2Seq.addBI(conclusion, conditional1Node, conditional2Node)
              DeductionSuccess(concNode, concSeq, conditional2Stack)
            case _ => DeductionFailure
          }
        case _ => DeductionFailure
      }
    }
  }

  def inferBE_Soft(request: DeductionRequest): DeductionResult = continueWithFormula[Conditional](request.conclusion) { cond =>
    request.sequence.findInDeduction(f => f match {
      case DeductionNode(Biconditional(left, right), _) => (left == cond.left && right == cond.right) || (right == cond.left && left == cond.right)
      case _ => false
    }) match {
      case Some((biconNode, biconSequence)) =>
        val (concNode, concSeq) = biconSequence.addBE(request.conclusion, biconNode)
        DeductionSuccess(concNode, concSeq, request.stack)
      case _ => DeductionFailure
    }
  }

  def inferCP_Soft(request: DeductionRequest): DeductionResult = continueWithFormula[Conditional](request.conclusion) { conditional =>
    val antecedent = conditional.left
    val consequent = conditional.right
    val (assumption, assumptionSeq) = request.sequence.addAssumptionCP(antecedent)
    inferSoft(DeductionRequest(consequent, assumptionSeq, request.stack)) match {
      case DeductionFailure => DeductionFailure
      case DeductionSuccess(consequentNode, consequenceSeq, consequentStack) =>
        val (concNode, concSeq) = consequenceSeq.addCP(request.conclusion, assumption, consequentNode)
        DeductionSuccess(concNode, concSeq, consequentStack)
    }
  }

  def inferDNI_Soft(request: DeductionRequest) = continueWithFormula[Negation](request.conclusion) { neg1 =>
    continueWithFormula[Negation](neg1.formula) { negation2 =>
      val (conclusion, sequence, stack) = request.decompose
      inferSoft(DeductionRequest(negation2.formula, sequence, stack)) match {
        case DeductionSuccess(dneNode, dneSeq, dneStack) =>
          val (concNode, concSeq) = dneSeq.addDNI(conclusion, dneNode)
          DeductionSuccess(concNode, concSeq, dneStack)
        case DeductionFailure => DeductionFailure
      }
    }
  }

  def inferDNE_Soft(request: DeductionRequest) = {
    val (conclusion, sequence,stack) = request.decompose

    val formulaToFind = Negation(Negation(conclusion))
    sequence.findInDeduction(formulaToFind) match {
      case Some((node, sequence2)) =>
        val (concNode, concSeq) = sequence2.addDNE(conclusion, node)
        DeductionSuccess(concNode, concSeq, stack)
      case _ => DeductionFailure
    }
  }

}
