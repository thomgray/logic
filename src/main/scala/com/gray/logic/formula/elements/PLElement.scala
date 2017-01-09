package com.gray.logic.formula.elements

abstract class LTerm(val index: Int)

// atomic formulas
case class LRelation(index: Int, args: Seq[LTerm]) extends LAtomicFormula with LRootElement
case class LEquals(left: LTerm, right: LTerm) extends LAtomicFormula with LRootElement

// terms
case class LConstant(override val index: Int) extends LTerm(index)
case class LVariable(override val index: Int) extends LTerm(index)
case class LFunction(override val index: Int, args: Seq[LTerm]) extends LTerm(index)
