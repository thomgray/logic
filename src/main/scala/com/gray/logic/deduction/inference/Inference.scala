package com.gray.logic.deduction.inference

import com.gray.logic.deduction._
import com.gray.logic.formula.Formula
import com.gray.logic.mixin.ControlFlow

import scala.reflect._

trait Inference extends ControlFlow{

//  val sequence: DeductionSequence

  def infer(conclusion: Formula, sequence: DeductionSequence): DeductionResult

  def doFirstSuccessful(formula: Formula)(seq: Seq[(Formula) => Option[DeductionNode]]): Option[DeductionNode] = {
    seq.find(block => block(formula) match {
      case Some(result) => return Some(result)
      case _ => false
    })
    None
  }

  def doFirstSuccessful2(deductionRequest: DeductionRequest)(seq: Seq[(DeductionRequest) => DeductionResult]) = {
    var result: DeductionResult = DeductionFailure
    seq.find(block => block(deductionRequest) match {
      case succeess: DeductionSuccess => result = succeess; true
      case _ => false
    })
    result
  }

  def continueWithFormula[T <: Formula :ClassTag](formula: Formula)(block: (T) => DeductionResult) = formula match {
    case t if classTag[T].runtimeClass.isInstance(t) => block(t.asInstanceOf[T])
    case _ => DeductionFailure
  }

}
