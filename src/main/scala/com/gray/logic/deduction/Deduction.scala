package com.gray.logic.deduction

import com.gray.logic.deduction.inference.{DeductionStack, Inference}
import com.gray.logic.formula.Formula
import com.gray.logic.language.FormulaWriter
import com.gray.logic.mixin.ControlFlow

class Deduction(val conclusion: Formula, val premises: Seq[Formula]) extends ControlFlow with Inference {

  private var sequence = DeductionSequence(premises: _*)

  def prove(conclusion: Formula) = {
    val request = DeductionRequest(conclusion, DeductionSequence(premises: _*),DeductionStack.empty)
    infer(request) match {
      case DeductionSuccess(node, seq, _) => sequence = seq
        Some(node, sequence)
      case _ => sequence = DeductionSequence(premises:_*)
        None
    }
  }

  def write(implicit formulaWriter: FormulaWriter) = {
    val premiseString = if (premises == Nil) "" else premises.map(_.write).mkString(",") + " "
    val concString = conclusion.write
    val title = s"$premiseString⊢ $concString"
    title + "\n\n" + sequence.write
  }

  override def infer(request: DeductionRequest): DeductionResult = DeductionFailure

}
