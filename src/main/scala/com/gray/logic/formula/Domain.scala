package com.gray.logic.formula

import scala.collection.mutable.{Set => MutableSet}

class Domain() {

  private val _sentences: MutableSet[Int] = MutableSet()
  private val _relations: MutableSet[Int] = MutableSet()

  private val _variables: MutableSet[Int] = MutableSet()
  private val _constants: MutableSet[Int] = MutableSet()
  private val _functions: MutableSet[Int] = MutableSet()

  def include(formula: Formula): Unit = formula match {
    case Sentence(index) => _sentences add index
    case Relation(index, terms) =>
      _relations add index
      terms.foreach(include)
    case Equals(left, right) =>
      include(left)
      include(right)
  }

  def include(term: Term): Unit = term match {
    case Constant(index) => _constants add index
    case Variable(index) => _variables add index
    case Function(index, terms) =>
      _functions add index
      terms foreach include
  }

  private def findNewIndex(collection: MutableSet[Int], index: Int = 0): Int = collection.contains(index) match {
    case true => findNewIndex(collection, index+1)
    case false =>
      collection add index
      index
  }

  def oldSentence = Sentence(_sentences.headOption getOrElse 0)

  def newVariable = Variable(findNewIndex(_variables))
  def newConstant = Constant(findNewIndex(_constants))
  def newFunction = FunctionScheme(findNewIndex(_functions))

  def newSentence = Sentence(findNewIndex(_sentences))
  def newRelation = RelationScheme(findNewIndex(_relations))

}
