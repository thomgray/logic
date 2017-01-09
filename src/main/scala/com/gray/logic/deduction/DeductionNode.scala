package com.gray.logic.deduction

import com.gray.logic.deduction.InferenceRules._
import com.gray.logic.formula.LFormula

class DeductionNode(val formula: LFormula, val inferenceRule: InferenceRule, val inferences: Seq[DeductionNode]) {

  private [deduction] var dependencies: Seq[DeductionNode] = findDependencies()

  private def findDependencies() = {
    inferenceRule match {
      case Premise => Seq(this)
      case ass if Assumptions.contains(ass) => Seq(this)
      case _ =>
        val inferencesToInherit = inferenceRule match {
          case CP =>
            val assumptionFormula = formula.composition.head
            inferences.filterNot(node => node.formula==assumptionFormula && node.inferenceRule==AssumptionCP)
          case RAA =>
            inferences.filterNot(node => node.inferenceRule==AssumptionRAA)
          case _ => inferences
        }
        inferencesToInherit.flatMap(_.inferences).distinct
    }
  }


}

object DeductionNode {
  def apply(formula: LFormula, inferenceRule: InferenceRule, inferences: Seq[DeductionNode] = Nil): DeductionNode = new DeductionNode(formula, inferenceRule, inferences)
}
