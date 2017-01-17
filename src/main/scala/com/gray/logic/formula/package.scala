package com.gray.logic

package object formula {

  object ConnectiveType extends Enumeration {
    val Conjunction, Disjunction, Conditional, Biconditional, Negation = Value
  }

  object QuantifierType extends Enumeration {
    val UniversalQuantifier, ExistentialQuantifier = Value
  }

}
