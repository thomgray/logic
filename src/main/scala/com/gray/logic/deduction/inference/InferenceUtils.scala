package com.gray.logic.deduction.inference

import com.gray.logic.deduction.DeductionSequence
import com.gray.logic.formula._
import com.gray.logic.operation.Extraction

trait InferenceUtils extends Extraction {

  def getDistinctFormulas(deductionSequence: DeductionSequence) = deductionSequence.nodes.map(_.formula).distinct

  def allFormulaDecompositions(deductionSequence: DeductionSequence): Seq[Formula] = allFormulaDecompositions(getDistinctFormulas(deductionSequence))

  def getDisjunctions(sequence: DeductionSequence) = allFormulaDecompositions(sequence) collect {
    case dj: Disjunction => dj
  }

  def getConditionals(sequence: DeductionSequence, f: (Conditional) => Boolean) = allFormulaDecompositions(sequence) collect{
    case cond: Conditional if f(cond) => cond
  }

  def getNegations(sequence: DeductionSequence) = allFormulaDecompositions(sequence) collect {
    case negation: Negation => negation
  }

  def getConjunctionsForCE(sequence: DeductionSequence, conclusion: Formula) = allFormulaDecompositions(sequence) collect {
    case conjunction@Conjunction(left, right) if left==conclusion || right==conclusion => conjunction
  }



  // TODO: refine this!
  def getFormulasForRAA(sequence: DeductionSequence) = allFormulaDecompositions(sequence).filter{
    case Negation(_) => false
    case _ => true
  }.sortWith{(f1,f2) =>
    f1.decompositions.length <= f2.decompositions.length
  }

}

object InferenceUtils extends InferenceUtils
