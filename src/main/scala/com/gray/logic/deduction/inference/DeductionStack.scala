package com.gray.logic.deduction.inference

import com.gray.logic.deduction.{DeductionFailure, DeductionResult, InferenceRule}
import com.gray.logic.formula.{Disjunction, Formula}

class DeductionStack (stack: Seq[DeductionStackNode], disjunctions: Seq[Disjunction]){

  def isRestricted(conclusion: Formula, rule: InferenceRule.Value) = !isPermitted(conclusion, rule)

  def isPermitted(conclusion: Formula, rule: InferenceRule.Value) =  !stack.exists{
    case DeductionStackNode(stackConc, Some(stackRule)) => stackConc == conclusion && stackRule == rule
    case DeductionStackNode(stackConc, None) => stackConc == conclusion
  }

  def isPermittedDisjunctionElimination(disjunction: Disjunction) = !disjunctions.contains(disjunction)

  def addRestriction(conclusion: Formula, rule: InferenceRule.Value) = {
    val newStack = stack :+ DeductionStackNode(conclusion, Some(rule))
    new DeductionStack(newStack, disjunctions)
  }

  def addDisjunctionRestriction(disjunction: Disjunction) = new DeductionStack(stack, disjunctions :+ disjunction)

  def check(conclusion: Formula, rule: InferenceRule.Value)(block: (DeductionStack) => DeductionResult) = if (isPermitted(conclusion, rule)){
    block(addRestriction(conclusion, rule))
  } else DeductionFailure

  def checkDisjunction(disjunction: Disjunction)(block: (DeductionStack) => DeductionResult) = if (isPermittedDisjunctionElimination(disjunction)){
    block(addDisjunctionRestriction(disjunction))
  } else DeductionFailure

}

case class DeductionStackNode(conclusion: Formula, ruleOption: Option[InferenceRule.Value])


