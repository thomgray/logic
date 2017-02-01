package com.gray.logic.deduction.inference

import com.gray.logic.deduction._
import com.gray.logic.formula._
import com.gray.logic.language.FormulaWriterAlphabetic
import com.gray.logic.mixin.ControlFlow

trait InferenceSoft extends Inference {

  override def infer(request: DeductionRequest) = inferSoft(request)

  private def inferSoft(request: DeductionRequest): Result = {
    request.sequence.findInDeduction(request.conclusion) match {
      case Some((node, sequence)) => Proven(node, sequence, request.stack)
      case _ =>
        doFirstSuccessful(request)(Seq(
          inferCI_Soft, inferCE_Soft,
          inferBI_Soft, inferBE_Soft,
          inferDNI_Soft, inferDNE_Soft,
          inferMP_Soft, inferMT_Soft,
          inferCP_Soft,
          inferDI_Soft
        ))
    }
  }

  def inferMP_Soft(deductionRequest: DeductionRequest): Result = {
    val conditionals = deductionRequest.sequence.findAllInDeduction(node => node.formula match {
      case Conditional(ant, cons) => cons == deductionRequest.conclusion
      case _ => false
    })

    var result: Result = Unproven

    conditionals.find { tuple =>
      val (conditionalNode, conditionalSequence) = tuple
      val antecedent = conditionalNode.formula.asInstanceOf[Conditional].left

      conditionalSequence.findInDeduction(antecedent) match {
        case Some((antecedentNode, antecedentSequence)) =>
          val (concNode, concSequence) = antecedentSequence.addMP(deductionRequest.conclusion, conditionalNode, antecedentNode)
          result = Proven(concNode, concSequence, deductionRequest.stack)
          true
        case _ => false
      }
    }
    result
  }


  def inferMT_Soft(request: DeductionRequest): Result = continueWith[Negation](request.conclusion){ negation =>
    val (conclusion, sequence, stack) = request.decompose
    val conditionals = sequence.findAllInDeduction ( node => node match {
      case DeductionNode(Conditional(ant,cons), _) => ant==negation.formula
      case _ => false
    })
    var result: Result = Unproven
    conditionals.find{ tuple =>
      val (conditionalNode, conditionalSeq) = (tuple._1, tuple._2)
      val conditionalFormula = conditionalNode.formula.asInstanceOf[Conditional]
      conditionalSeq.findInDeduction(Negation(conditionalFormula.right)) match {
        case None => false
        case Some((negConsNode, negConsSeq)) =>
          val (concNode, concSeq) = negConsSeq.addMT(conclusion, conditionalNode, negConsNode)
          result = Proven(concNode, concSeq, stack)
          true
      }
    }
    result
  }


  def inferCI_Soft(request: DeductionRequest): Result = continueWith[Conjunction](request.conclusion) { conjunction =>
    val (left, right) = (conjunction.left, conjunction.right)
    inferSoft(DeductionRequest(left, request.sequence, request.stack)) match {
      case Unproven => Unproven
      case Proven(leftNode, leftSeq, leftStack) =>
        inferSoft(DeductionRequest(right, leftSeq, leftStack)) match {
          case Unproven => Unproven
          case Proven(rightNode, rightSeq, rightStack) =>
            val (concNode, concSeq) = rightSeq.addCI(request.conclusion, leftNode, rightNode)
            Proven(concNode, concSeq, rightStack)
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
        Proven(concNode, concSeq, stack)
      case _ => Unproven
    }
  }

  def inferDI_Soft(request: DeductionRequest): Result = {
    val (conclusion, sequence, stack) = request.decompose
    continueWith[Disjunction](conclusion) { disjunction =>
      val (dj1, dj2) = (disjunction.left, disjunction.right)
      inferSoft(DeductionRequest(dj1, sequence, stack)) match {
        case Proven(dj1Node, dj1Seq, dj1Stack) =>
          val (concNode, concSeq) = dj1Seq.addDI(conclusion, dj1Node)
          Proven(concNode, concSeq, stack)
        case Unproven =>
          inferSoft(DeductionRequest(dj2, sequence, stack)) match {
            case Proven(dj2Node, dj2Seq, dj2Stack) =>
              val (concNode, concSeq) = dj2Seq.addDI(conclusion, dj2Node)
              Proven(concNode, concSeq, stack)
            case _ => Unproven
          }
      }
    }
  }

  def inferDE_Soft(request: DeductionRequest): Result = Unproven

  def inferBI_Soft(request: DeductionRequest) = {
    val (conclusion, sequence, stack) = request.decompose
    continueWith[Biconditional](conclusion) { biconditional =>
      val conditional1 = Conditional(biconditional.left, biconditional.right)
      val conditional2 = Conditional(biconditional.right, biconditional.left)

      inferSoft(DeductionRequest(conditional1, sequence, stack)) match {
        case Proven(conditional1Node, conditional1Seq, conditional1Stack) =>
          inferSoft(DeductionRequest(conditional2, conditional1Seq, conditional1Stack)) match {
            case Proven(conditional2Node, conditional2Seq, conditional2Stack) =>
              val (concNode, concSeq) = conditional2Seq.addBI(conclusion, conditional1Node, conditional2Node)
              Proven(concNode, concSeq, conditional2Stack)
            case _ => Unproven
          }
        case _ => Unproven
      }
    }
  }

  def inferBE_Soft(request: DeductionRequest): Result = continueWith[Conditional](request.conclusion) { cond =>
    request.sequence.findInDeduction(f => f match {
      case DeductionNode(Biconditional(left, right), _) => (left == cond.left && right == cond.right) || (right == cond.left && left == cond.right)
      case _ => false
    }) match {
      case Some((biconNode, biconSequence)) =>
        val (concNode, concSeq) = biconSequence.addBE(request.conclusion, biconNode)
        Proven(concNode, concSeq, request.stack)
      case _ => Unproven
    }
  }

  def inferCP_Soft(request: DeductionRequest): Result = continueWith[Conditional](request.conclusion) { conditional =>
    val antecedent = conditional.left
    val consequent = conditional.right
    val (assumption, assumptionSeq) = request.sequence.addAssumptionCP(antecedent)
    inferSoft(DeductionRequest(consequent, assumptionSeq, request.stack)) match {
      case Unproven => Unproven
      case Proven(consequentNode, consequenceSeq, consequentStack) =>
        val (concNode, concSeq) = consequenceSeq.addCP(request.conclusion, assumption, consequentNode)
        Proven(concNode, concSeq, consequentStack)
    }
  }

  def inferDNI_Soft(request: DeductionRequest) = continueWith[Negation](request.conclusion) { neg1 =>
    continueWith[Negation](neg1.formula) { negation2 =>
      val (conclusion, sequence, stack) = request.decompose
      inferSoft(DeductionRequest(negation2.formula, sequence, stack)) match {
        case Proven(dneNode, dneSeq, dneStack) =>
          val (concNode, concSeq) = dneSeq.addDNI(conclusion, dneNode)
          Proven(concNode, concSeq, dneStack)
        case Unproven => Unproven
      }
    }
  }

  def inferDNE_Soft(request: DeductionRequest) = {
    val (conclusion, sequence, stack) = request.decompose
    val formulaToFind = Negation(Negation(conclusion))
    sequence.findInDeduction(formulaToFind) match {
      case Some((node, sequence2)) =>
        val (concNode, concSeq) = sequence2.addDNE(conclusion, node)
        Proven(concNode, concSeq, stack)
      case _ => Unproven
    }
  }

}
