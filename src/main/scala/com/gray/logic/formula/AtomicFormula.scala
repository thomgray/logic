package com.gray.logic.formula

case class Relation(index: Int, args: Seq[Term] = Nil) extends Formula {
  def apply(terms: Term*) = Relation(index, terms)
}

case class RelationScheme(index: Int) {
  def apply(terms: Term*): Relation = Relation(index, terms)
}

case class Equals(left: Term, right: Term) extends Formula

case class Sentence(index: Int) extends Formula