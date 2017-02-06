package com.gray.logic.deduction.inference

import com.gray.logic.deduction.InferenceRule
import com.gray.logic.formula.{Disjunction, Formula}
import com.gray.logic.language.FormulaWriter
import com.gray.logic.tools.Logging

class DeductionStack(stack: Seq[DeductionStackNode], disjunctions: Seq[Disjunction]) extends Logging {

  def isRestricted(conclusion: Formula, rule: InferenceRule.Value) = !isPermitted(conclusion, rule)

  def isPermitted(conclusion: Formula, rule: InferenceRule.Value) = !stack.exists {
    case DeductionStackNode(stackConc, Some(stackRule)) => stackConc == conclusion && stackRule == rule
    case DeductionStackNode(stackConc, None) => stackConc == conclusion
  }

  def isPermittedDisjunctionElimination(disjunction: Disjunction) = !disjunctions.contains(disjunction)

  def addRestriction(conclusion: Formula, rule: InferenceRule.Value) = {
    val newStack = stack :+ DeductionStackNode(conclusion, Some(rule))
    DeductionStack(newStack, disjunctions)
  }

  def addDisjunctionRestriction(disjunction: Disjunction) = new DeductionStack(stack, disjunctions :+ disjunction)

  def check(conclusion: Formula, rule: InferenceRule.Value)(block: (DeductionStack) => Result) = rule match {
    case InferenceRule.DE => block(this)
    case _ =>
      if (isPermitted(conclusion, rule)) {
        block(addRestriction(conclusion, rule))
      } else {
        logger.info(s"Blocking inference to [${
          conclusion.write
        }] for rule $rule")
        Unproven
      }
  }

  def checkDisjunction(disjunction: Disjunction)(block: (DeductionStack) => Result) = if (isPermittedDisjunctionElimination(disjunction)) {
    block(addDisjunctionRestriction(disjunction))
  } else Unproven

  def write(implicit formulaWriter: FormulaWriter) = {
    (stack.map(node => s"${
      node.conclusion.write(formulaWriter)
    } : ${
      node.ruleOption.getOrElse("all")
    }") ++ disjunctions.map(dj => s"${
      dj.write(formulaWriter)
    } for DE")).mkString("\n")
  }

}

object DeductionStack {
  def apply(stack: Seq[DeductionStackNode], disjunctions: Seq[Disjunction]): DeductionStack = new DeductionStack(stack, disjunctions)

  def empty() = new DeductionStack(Nil, Nil)
}

case class DeductionStackNode(conclusion: Formula, ruleOption: Option[InferenceRule.Value])


