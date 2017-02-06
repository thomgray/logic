package com.gray.logic.formula

trait AtomicFormula extends Formula
object AtomicFormula {
  def unapply(arg: Formula): Option[Seq[Term]] = arg match {
    case Sentence(i) => Some(Nil)
    case Relation(i, terms) => Some(terms)
    case Equals(l,r) => Some(Seq(l,r))
    case _ => None
  }
}

case class Relation(index: Int, args: Seq[Term] = Nil) extends AtomicFormula {
  def apply(terms: Term*) = Relation(index, terms)
}

case class RelationScheme(index: Int) {
  def apply(terms: Term*): Relation = Relation(index, terms)
}

case class Equals(left: Term, right: Term) extends AtomicFormula

case class Sentence(index: Int) extends AtomicFormula