package com.gray.logic.deduction

import com.gray.logic.deduction.inference.DeductionStack
import com.gray.logic.formula.Formula

abstract class DeductionResult

case class DeductionSuccess(deductionNode: DeductionNode, sequence: DeductionSequence, stack: DeductionStack) extends DeductionResult

case object DeductionFailure extends DeductionResult

case class DeductionRequest(conclusion: Formula, sequence: DeductionSequence, stack: DeductionStack) {
  def decompose = (conclusion, sequence, stack)
}

case object DeductionStackDud extends DeductionStack(Nil, Nil)