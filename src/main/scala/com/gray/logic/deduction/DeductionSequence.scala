package com.gray.logic.deduction

import com.gray.logic.formula._
import com.gray.logic.language.FormulaWriter

class DeductionSequence(nodes: Seq[DeductionNode], val tier: Int) {

  import InferenceRule._

  def stepUp = copy(tier = tier + 1)

  def stepDown = copy(tier = tier - 1)

  def length = nodes.length

  def visibleNodes = {
    var _tier = tier
    val visibleNodes = for (node <- nodes.reverse.toList if node.tier <= _tier) yield {
      _tier = node.tier
      if (InferenceRule.assumptions.contains(node.inferenceRule)) _tier -= 1
      node
    }
    visibleNodes.reverse
  }

  def findInDeduction(formula: Formula) = visibleNodes.find(_.formula == formula) match {
    case Some(node) if node.tier < tier =>
      val reiteration = addNode(DeductionNode(node.formula, InferenceRule.Reiteration, Seq(node), tier))
      Some(reiteration)
    case Some(other) => Some(other, this)
    case None => None
  }

  def findInDeduction(f: (DeductionNode) => Boolean) = visibleNodes.find(f) match {
    case Some(node) if node.tier < tier =>
      val reiteration = addNode(DeductionNode(node.formula, InferenceRule.Reiteration, Seq(node), tier))
      Some(reiteration)
    case Some(other) => Some(other, this)
    case None => None
  }

  def findAllInDeduction(formula: Formula) = visibleNodes.filter(_.formula == formula) map {
    case node if node.tier < tier =>
      val reiteration = addNode(DeductionNode(node.formula, InferenceRule.Reiteration, Seq(node), tier))
      reiteration
    case other => (other, this)
  }

  def findAllInDeduction(f: (DeductionNode) => Boolean) = visibleNodes filter f map {
    case node if node.tier < tier =>
      val reiteration = addNode(DeductionNode(node.formula, InferenceRule.Reiteration, Seq(node), tier))
      reiteration
    case other => (other, this)
  }

  //todo
  def findAllInDeduction(partialFunction: PartialFunction[DeductionNode, Boolean]) = visibleNodes filter { node =>
    if (partialFunction.isDefinedAt(node)) partialFunction(node)
    else false
  }


  def appendLine(node: DeductionNode, tier: Int = this.tier) = {
    val newNodes = nodes :+ node
    (node, copy(newNodes))
  }

  private def addNode(node: DeductionNode, tier: Int = this.tier) = {
    val newNodes = nodes :+ node
    (node, copy(newNodes, tier))
  }

  def addMP(conclusion: Formula, conditional: DeductionNode, antecedent: DeductionNode) = addNode(DeductionNode(conclusion, MP, Seq(conditional, antecedent), tier))

  def addCP(conclusion: Formula, assumption: DeductionNode, minorConc: DeductionNode) = {
    addNode(DeductionNode(conclusion, CP, Seq(assumption, minorConc), tier).dischargeDependency(assumption), tier - 1)
  }

  def addCI(conclusion: Formula,
            left: DeductionNode,
            right: DeductionNode) = addNode(DeductionNode(conclusion, CI, Seq(left, right), tier))

  def addCE(conclusion: Formula,
            conjunction: DeductionNode) = addNode(DeductionNode(conclusion, CE, Seq(conjunction), tier))

  def addDI(conclusion: Formula, disjunct: DeductionNode) = addNode(DeductionNode(conclusion, DI, Seq(disjunct), tier))

  def addDE(conclusion: Formula,
            disjunction: DeductionNode,
            assunption1: DeductionNode,
            conc1: DeductionNode,
            assumption2: DeductionNode,
            conc2: DeductionNode) = addNode(DeductionNode(conclusion, DE, Seq(disjunction, assunption1, conc1, assumption2, conc2), tier - 1), tier - 1)

  def addBI(conclusion: Formula,
            conditional1: DeductionNode,
            conditizonal2: DeductionNode) = addNode(DeductionNode(conclusion, BI, Seq(conditional1, conditizonal2), tier))

  def addBE(conclusion: Formula,
            biconditional: DeductionNode) = addNode(DeductionNode(conclusion, BE, Seq(biconditional), tier))

  def addDNE(conclusion: Formula,
             node: DeductionNode) = addNode(DeductionNode(conclusion, DNE, Seq(node), tier))

  def addDNI(conclusion: Formula,
             node: DeductionNode) = addNode(DeductionNode(conclusion, DNI, Seq(node), tier))


  private def addAssumption(assumption: Formula, rule: InferenceRule.Value) = {
    val node = new DeductionNode(assumption, rule, Nil, None, tier + 1)
    addNode(node, tier + 1)
  }

  def addAssumptionCP(assumption: Formula) = addAssumption(assumption, InferenceRule.AssCP)

  def addAssumptionDE(assumption: Formula) = addAssumption(assumption, InferenceRule.AssDE)

  def addAssumptionRAA(assumption: Formula) = addAssumption(assumption, InferenceRule.AssRAA)

  private def lineForNode(deductionNode: DeductionNode) = nodes.indexOf(deductionNode) + 1

  private def stringSegmentsForNodes(writer: FormulaWriter) = nodes.toList.map { node =>
    def indent(i: Int, sofar: String = ""): String = if (i == 0) sofar else indent(i - 1, sofar + "  ")
    val formulaString = indent(node.tier) + node.formula.write(writer)

    val depString = "{" + node.dependencies.map(lineForNode).mkString(",") + "}"

    val inferenceString = node.inferences.map(lineForNode).mkString(",")
    val inferenceStringWithRule = (if (inferenceString.isEmpty) "" else inferenceString + " ") + InferenceRule.string(node.inferenceRule)
    (lineForNode(node).toString, depString, formulaString, inferenceStringWithRule)
  }

  def write(implicit formulaWriter: FormulaWriter) = {
    val stringSegments = stringSegmentsForNodes(formulaWriter)

    var lineWidth, dependenciesWidth, formulaWidth, inferneceWidth = 0
    stringSegments foreach { tuple =>
      val (line, dependencies, formula, inference) = tuple
      if (line.length > lineWidth) lineWidth = line.length
      if (dependencies.length > dependenciesWidth) dependenciesWidth = dependencies.length
      if (formula.length > formulaWidth) formulaWidth = formula.length
      if (inference.length > inferneceWidth) inferneceWidth = inference.length
    }

    val regularWidthStrings = stringSegments map { tuple =>
      val (line, dependencies, formula, inference) = tuple
      (line.padTo(lineWidth, ' '), dependencies.padTo(dependenciesWidth, ' '), formula.padTo(formulaWidth, ' '), inference.padTo(inferneceWidth, ' '))
    }

    val lines = regularWidthStrings map { tuple =>
      val (line, dependencies, formula, inference) = tuple
      s"$line $dependencies $formula   $inference"
    }
    lines.mkString("\n")
  }

  def getDistinctFormulas = nodes.toList.map(_.formula).distinct


  def copy(nodes: Seq[DeductionNode] = this.nodes, tier: Int = this.tier) = new DeductionSequence(nodes, tier)
}

object DeductionSequence {
  def apply(premises: Formula*): DeductionSequence = {
    val premiseNodes = premises map (DeductionNode(_, InferenceRule.Premise, Nil))
    new DeductionSequence(premiseNodes, 0)
  }
}
