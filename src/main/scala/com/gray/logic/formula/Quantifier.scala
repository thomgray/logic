package com.gray.logic.formula

case class UniversalQuantifier(override val variable: Variable, override val formula: Formula) extends Quantifier(variable, formula)

case class ExistentialQuantifier(override val variable: Variable, override val formula: Formula) extends Quantifier(variable, formula)