package com.gray.logic.formula

import scala.collection.mutable.{Set => MutableSet}
import scala.collection.mutable.{Map => MutableMap}

class Domain() {


  private val sentences: MutableSet[Sentence] = MutableSet()
  private val relations: MutableSet[RelationScheme] = MutableSet()
  private val constants: MutableSet[Constant] = MutableSet()
  private val variables: MutableSet[Variable] = MutableSet()
  private val functions: MutableSet[FunctionScheme] = MutableSet()

  private val namedConstants: MutableMap[String, Constant] = MutableMap()
  private val namedRelations: MutableMap[String, RelationScheme] = MutableMap()
  private val namedSentences: MutableMap[String, Sentence] = MutableMap()


  private def findNewIndex(collection: MutableSet[Int], index: Int = 0): Int = collection.contains(index) match {
    case true => findNewIndex(collection, index + 1)
    case false =>
      collection add index
      index
  }

  def namedConstant(string: String) = namedConstants.applyOrElse(string, { s: String =>
    val constant = newConstant
    namedConstants(s) = constant
    constant
  })


  def namedRelation(string: String) = namedRelations.applyOrElse(string, { s: String =>
    val relation = newRelation
    namedRelations(s) = relation
    relation
  })

  def namedSentence(string: String) = namedSentences.applyOrElse(string, {s: String =>
    val sentence = newSentence
    namedSentences(s) = sentence
    sentence
  })

  def oldSentence = sentences.headOption getOrElse newSentence

  def oldConstant = constants.headOption getOrElse newConstant

  def newVariable = {
    def index = findNewIndex(variables.map(_.index))
    val variable = Variable(index)
    variables add variable
    variable
  }

  def newConstant = {
    val index = findNewIndex(constants.map(_.index))
    val constant = Constant(index)
    constants add constant
    constant
  }

  def newFunction = {
    val index = findNewIndex(functions.map(_.index))
    val function = FunctionScheme(index)
    functions add function
    val t = FunctionScheme
    function
  }

  def newSentence = {
    val index = findNewIndex(sentences.map(_.index))
    val sentence = Sentence(index)
    sentences add sentence
    sentence
  }

  def newRelation = {
    val index = findNewIndex(relations.map(_.index))
    val relation = RelationScheme(index)
    relations add relation
    relation
  }

}
