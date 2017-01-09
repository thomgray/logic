package com.gray.logic.formula.elements

abstract class LCompositor extends LElement with LRootElement {
  def isConnective = false
  def isQuantifier = false

  def asConnective = this match {
    case conn: LConnective => Some(conn)
    case _ => None
  }

  def asQuantifier = this match {
    case quantifier: LQuantifier => Some(quantifier)
    case _ => None
  }
}


case class LConnective(connectiveType: LConnectiveType.ConnectiveType) extends LCompositor {
  override def isConnective = true
}

object LConnective {
  def conjunction = new LConnective(LConnectiveType.Conjunction)
  def disjunction = new LConnective(LConnectiveType.Disjunction)
  def negation = new LConnective(LConnectiveType.Negation)
  def conditional = new LConnective(LConnectiveType.Conditional)
  def biconditional = new LConnective(LConnectiveType.Biconditional)
}

case class LQuantifier(quantifierType: LQuantifierType.QuantifierType, variable: LVariable) extends LCompositor {
  override def isQuantifier: Boolean = true
}

object LQuantifier {
  def existential(variable: LVariable) = new LQuantifier(LQuantifierType.Existential, variable)
  def universal(variable: LVariable) = new LQuantifier(LQuantifierType.Universal, variable)
}

object LQuantifierType extends Enumeration{
  type QuantifierType = Value
  val Existential, Universal = Value
}

object LConnectiveType extends Enumeration {
  type ConnectiveType = Value
  val Conjunction, Disjunction, Negation, Conditional, Biconditional = Value
}




