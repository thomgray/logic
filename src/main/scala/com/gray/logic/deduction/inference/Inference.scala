package com.gray.logic.deduction.inference

import com.gray.logic.deduction._
import com.gray.logic.formula.Formula

import scala.reflect._

trait Inference extends InferenceDirectives {

  def infer(request: DeductionRequest): Result

  def doFirstSuccessful(deductionRequest: DeductionRequest)(seq: Seq[(DeductionRequest) => Result]): Result = {
//    val functions = seq map (f => () => f(deductionRequest))
//    firstSuccess(functions)
    seq foreach (f => f(deductionRequest) match {
      case proven:Proven => return proven
      case _ =>
    })
    Unproven
  }

  def doFirstSuccessful(functions: Seq[()=> Result]) : Result = {
    functions foreach(f => f() match {
      case proven: Proven => return proven
      case _ =>
    })
    Unproven
  }

  def continueWith[T <: Formula : ClassTag](formula: Formula)(block: (T) => Result) = formula match {
    case t if classTag[T].runtimeClass.isInstance(t) => block(t.asInstanceOf[T])
    case _ => Unproven
  }

  def continueIf[T](condition: Boolean)(block: () => Result) = if (condition) block() else Unproven

}
