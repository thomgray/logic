package com.gray.logic.deduction.inference

import com.gray.logic.deduction.{DeductionSequence, InferenceRule}
import com.gray.logic.formula.Formula

class DeductionRequest(val conclusion: Formula, val sequence: DeductionSequence, val stack: DeductionStack) {
  def decompose = (conclusion, sequence, stack)
  def check(rule: InferenceRule.Value)(block: (DeductionStack) => Result) = stack.check(conclusion, rule)(block)

}

object DeductionRequest {
  def apply(conclusion: Formula, sequence: DeductionSequence, stack: DeductionStack): DeductionRequest = new DeductionRequest(conclusion, sequence, stack)
  def unapply(arg: DeductionRequest): Option[(Formula, DeductionSequence, DeductionStack)] = Some(arg.conclusion, arg.sequence, arg.stack)
}