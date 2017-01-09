package com.gray.logic.deduction

import com.gray.logic.formula.{FormulaWriter, LFormula}
import com.gray.logic.deduction.InferenceRules._

import scala.collection.mutable.ListBuffer

abstract class DeductionBase(conclusion: LFormula, premises: LFormula*) {

  protected [deduction] val nodes: ListBuffer[DeductionNode] = {
    val premiseNodes = premises map (DeductionNode(_, Premise))
    ListBuffer(premiseNodes:_*)
  }

  protected [deduction] def infer(lFormula: LFormula, rule: InferenceRule, inferences: Seq[DeductionNode]) = {
    val newNode = DeductionNode(lFormula, rule, inferences)
    nodes.append(newNode)
    newNode
  }

  protected [deduction] def findNode(lFormula: LFormula) = nodes find(_.formula == lFormula)
  protected [deduction] def findNode(f: (LFormula) => Boolean) = nodes find(node => f(node.formula))

  protected [deduction] def findNodes(f: (LFormula) => Boolean) = nodes.filter(node => f(node.formula)).toList
  protected [deduction] def findNodes(f: LFormula) = nodes.filter(node => node.formula==f).toList



  def write(implicit formulaWriter: FormulaWriter) = {
    var i = 0
    "\n" + nodes.map{node =>
      i += 1
      val formulaString = node.formula.write
      s"$i\t$formulaString\t${node.inferenceRule}\t"
    }.mkString("\n")
  }
}
