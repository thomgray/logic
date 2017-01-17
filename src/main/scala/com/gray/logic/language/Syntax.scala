package com.gray.logic.language

trait Syntax {

  //formulas
  def writeSentence(sentence: String) = sentence

  def writeRelation(relation: String, args: Seq[String]): String

  def writeEquals(equals: String, left: String, right: String): String

  //compositors
  def writeConjunction(token: String, left: String, right: String): String

  def writeDisjunction(token: String, left: String, right: String): String

  def writeConditional(token: String, left: String, right: String): String

  def writeBiconditional(token: String, left: String, right: String): String

  def writeNegation(token: String, formula: String): String

  def writeUniversalQuantifier(token: String, variable: String, formula: String): String

  def writeExistentialQuantifier(token: String, variable: String, formula: String): String

  //terms
  def writeVariable(string: String) = string

  def writeConstant(string: String) = string

  def writeFunction(function: String, args: Seq[String]): String


  ///reading

  /**
    * @param left  : the string to the left of the token
    * @param right : the string to the right of the token
    * @return
    * <ul>
    * <li><code>None</code> if the string is not an Equals</li>
    * <li><code>Some(leftTerm, rightTerm)</code> if the string is an equals
    * </ul>
    */
  def readEquals(left: String, right: String): Option[(String, String)]

  /**
    *
    * @param left  : the string to the left of the token
    * @param right : the string to the right of the token
    * @return
    * <ul>
    * <li><code>false</code> if the string is not a Sentence</li>
    * <li><code>true</code> if the string is a Sentence
    * </ul>
    */
  def readSentence(left: String, right: String): Boolean

  /**
    *
    * @param left  : the string to the left of the token
    * @param right : the string to the right of the token
    * @return
    * <ul>
    * <li><code>None</code> if the string is not a Relation</li>
    * <li><code>Some(List(argString))</code> if the string is a Relation
    * </ul>
    */
  def readRelation(left: String, right: String): Option[Seq[String]]

  /**
    *
    * @param left  : the string to the left of the token
    * @param right : the string to the right of the token
    * @return
    * <ul>
    * <li><code>false</code> if the string is not a Variable</li>
    * <li><code>true</code> if the string is a Variable
    * </ul>
    */
  def readVariable(left: String, right: String): Boolean

  /**
    *
    * @param left  : the string to the left of the token
    * @param right : the string to the right of the token
    * @return
    * <ul>
    * <li><code>false</code> if the string is not a Constant</li>
    * <li><code>true</code> if the string is a Constant
    * </ul>
    */
  def readConstant(left: String, right: String): Boolean

  /**
    *
    * @param left  : the string to the left of the token
    * @param right : the string to the right of the token
    * @return
    * <ul>
    * <li><code>None</code> if the string is not a Function</li>
    * <li><code>Some(List(termString))</code> if the string is a Function
    * </ul>
    */
  def readFunction(left: String, right: String): Option[Seq[String]]


  def readConjunction(left: String, right: String): Option[(String, String)]
  def readDisjunction(left: String, right: String): Option[(String, String)]
  def readConditional(left: String, right: String): Option[(String, String)]
  def readBiconditional(left: String, right: String): Option[(String, String)]
  def readNegation(left: String, right: String): Option[String]

  def readUniversalQuantifier(left: String, right: String): Option[(String, String)]
  def readExistentialQuantifier(left: String, right: String): Option[(String, String)]



}
