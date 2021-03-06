package com.gray.logic.deduction.inference

import com.gray.logic.deduction.{DeductionNode, DeductionSequence}

abstract class Result {
  def apply(f: (Proven) => Result): Result
  def apply(f: (DeductionNode, DeductionSequence) => Result): Result
}

case class Proven(deductionNode: DeductionNode, sequence: DeductionSequence) extends Result {
  override def apply(f: (Proven) => Result): Result = f(this)
  override def apply(f: (DeductionNode, DeductionSequence) => Result): Result = f(deductionNode, sequence)

  def decompose = (deductionNode, sequence)
}

case object Unproven extends Result {
  override def apply(f: (Proven) => Result): Result = this
  override def apply(f: (DeductionNode, DeductionSequence) => Result): Result = this
}

case object DeductionStackDud extends DeductionStack(Nil, Nil)