package com.gray.logic.deduction

import com.gray.logic.deduction.inference._
import com.gray.logic.formula.Formula
import com.gray.logic.language.FormulaWriter
import com.gray.logic.mixin.ControlFlow

class Deduction(val conclusion: Formula, val premises: Seq[Formula]) extends ControlFlow with Inference {

  private var sequence = DeductionSequence(premises: _*)

  def prove(conclusion: Formula) = {
    val request = DeductionRequest(conclusion, DeductionSequence(premises: _*),DeductionStack.empty)
    infer(request) match {
      case Proven(node, seq) => sequence = seq
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

  override def infer(request: DeductionRequest): Result = Unproven

}

object Deduction {
  def prove(conclusion: Formula, premises: Seq[Formula]) = {
    val deduction = new Deduction(conclusion, premises) with InferenceHard
    deduction.prove(conclusion) match {
      case Some((_, seq)) => seq
      case None => deduction.sequence
    }
  }
}