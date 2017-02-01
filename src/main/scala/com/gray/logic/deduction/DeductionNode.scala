package com.gray.logic.deduction

import com.gray.logic.formula.Formula

class DeductionNode(val formula: Formula, val inferenceRule: InferenceRule.Value, val inferences: Seq[DeductionNode], dependenciesOpt: Option[Seq[DeductionNode]] = None, val tier: Int = 0) {

  val dependencies: Seq[DeductionNode] = dependenciesOpt match {
    case Some(list) => list
    case None => findDependencies()
  }

  private def findDependencies() = inferenceRule match {
    case InferenceRule.Premise => Seq(this)
    case ass if InferenceRule.assumptions.contains(ass) => Seq(this)
    case _ => inferences.flatMap(_.dependencies).distinct
  }


  def dischargeDependency(nodes: DeductionNode*) = {
    val dependenciesNew = dependencies.filterNot(n=>nodes.contains(n))
    copy(newDependencies = Some(dependenciesNew))
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case formula: Formula => formula==this.formula
    case _ => super.equals(obj)
  }

  def copy(newFormula: Formula = formula, newInferenceRule: InferenceRule.Value = inferenceRule,newInferences: Seq[DeductionNode] = inferences, newDependencies: Option[Seq[DeductionNode]] = Some(dependencies), newTier: Int = tier) = new DeductionNode(newFormula, newInferenceRule,newInferences, newDependencies, newTier)


}

object DeductionNode {
  def apply(formula: Formula, inferenceRule: InferenceRule.Value, inferences: Seq[DeductionNode], tier: Int) = new DeductionNode(formula, inferenceRule, inferences, None, tier)
  def apply(formula: Formula, inferenceRule: InferenceRule.Value, inferences: Seq[DeductionNode], dependencies: Seq[DeductionNode]): DeductionNode = new DeductionNode(formula, inferenceRule, inferences, Some(dependencies))
  def apply(formula: Formula, inferenceRule: InferenceRule.Value, inferences: Seq[DeductionNode]): DeductionNode = new DeductionNode(formula, inferenceRule, inferences, None)

  def unapply(arg: DeductionNode): Option[(Formula, InferenceRule.Value)] = Some((arg.formula, arg.inferenceRule))

//  def premise(formula: Formula, tier: Int=0) = new DeductionNode(formula, InferenceRule.Premise, Nil, None, tier)
//  def assumptionCP(formula: Formula, tier: Int = 0) = new DeductionNode(formula, InferenceRule.AssCP, Nil, None, tier)
//  def assumptionRAA(formula: Formula,tier: Int = 0) = new DeductionNode(formula, InferenceRule.AssRAA, Nil, None, tier)
//  def assumptionDE(formula: Formula, tier: Int = 0) = new DeductionNode(formula, InferenceRule.AssDE, Nil, None, tier)
//
//  def modusPonens(formula: Formula, conditional: DeductionNode, antecedent: DeductionNode) = new DeductionNode(formula, InferenceRule.MP, Seq(conditional, antecedent))
//  def modusTollens(formula: Formula, conditional: DeductionNode, consequent: DeductionNode) = new DeductionNode(formula, InferenceRule.MT, Seq(conditional, consequent))
//
//  def conjunctionIntroduction(formula: Formula, left: DeductionNode, right: DeductionNode) = new DeductionNode(formula, InferenceRule.CI, Seq(left, right))
//  def conjunctionElimination(formula: Formula, node: DeductionNode) = new DeductionNode(formula, InferenceRule.CE, Seq(node))
//
//  def disjunctionIntroduction(formula: Formula, disjunct: DeductionNode) = new DeductionNode(formula, InferenceRule.DI, Seq(disjunct))
//
//  def biconditionalIntroduction(formula: Formula, condtitional1: DeductionNode, conditional2: DeductionNode) = new DeductionNode(formula, InferenceRule.BI, Seq(condtitional1, conditional2))
//  def biconditionalElimination(formula: Formula, biconditinal: DeductionNode) = new DeductionNode(formula, InferenceRule.BE, Seq(biconditinal))
}
