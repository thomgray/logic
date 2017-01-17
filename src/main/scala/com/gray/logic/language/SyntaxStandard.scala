package com.gray.logic.language

import com.gray.logic.mixin.{ControlFlow, ReadUtils}

object SyntaxStandard extends Syntax with ControlFlow with ReadUtils {
  override def writeRelation(relation: String, args: Seq[String]): String = s"$relation${args.mkString}"

  override def writeEquals(equals: String, left: String, right: String): String = s"$left$equals$right"

  //compositors
  override def writeConjunction(token: String, left: String, right: String): String = s"($left$token$right)"

  override def writeDisjunction(token: String, left: String, right: String): String = s"($left$token$right)"

  override def writeConditional(token: String, left: String, right: String): String = s"($left$token$right)"

  override def writeBiconditional(token: String, left: String, right: String): String = s"($left$token$right)"

  override def writeNegation(token: String, formula: String): String = s"$token$formula"

  override def writeUniversalQuantifier(token: String, variable: String, formula: String): String = s"$token$variable$formula"

  override def writeExistentialQuantifier(token: String, variable: String, formula: String): String = s"$token$variable$formula"

  override def writeFunction(function: String, args: Seq[String]): String = s"$function(${args.mkString})"


  /**
    * @param left  : the string to the left of the token
    * @param right : the string to the right of the token
    * @return
    * <ul>
    * <li><code>None</code> if the string is not an Equals</li>
    * <li><code>Some(leftTerm, rightTerm)</code> if the string is an equals
    * </ul>
    */
  override def readEquals(left: String, right: String): Option[(String, String)] = Some(left, right)

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
  override def readRelation(left: String, right: String): Option[Seq[String]] = ???

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
  override def readFunction(left: String, right: String): Option[Seq[String]] = continueIf(left.isEmpty && right.startsWith("(")) {
    rangeOfBrackets(right, 0) match {
      case Some((s,e)) if e==right.length => readTermList(right.substring(s+1, e-1))
      case _ => None
    }
  }

  // 'aRb' 'Rab'

  private def readTermList(string: String): Option[Seq[String]] = {
    None
  }

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
  override def readSentence(left: String, right: String): Boolean = left.isEmpty && right.isEmpty

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
  override def readVariable(left: String, right: String): Boolean = left.isEmpty && right.isEmpty

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
  override def readConstant(left: String, right: String): Boolean = left.isEmpty && right.isEmpty

  override def readConjunction(left: String, right: String): Option[(String, String)] = if (left.startsWith(")") && right.endsWith(")")) Some((left.stripPrefix("("), right.stripSuffix(")"))) else None

  override def readDisjunction(left: String, right: String): Option[(String, String)] = if (left.startsWith(")") && right.endsWith(")")) Some((left.stripPrefix("("), right.stripSuffix(")"))) else None

  override def readConditional(left: String, right: String): Option[(String, String)] = if (left.startsWith(")") && right.endsWith(")")) Some((left.stripPrefix("("), right.stripSuffix(")"))) else None

  override def readBiconditional(left: String, right: String): Option[(String, String)] = if (left.startsWith(")") && right.endsWith(")")) Some((left.stripPrefix("("), right.stripSuffix(")"))) else None

  override def readNegation(left: String, right: String): Option[String] = if (left.isEmpty && right.nonEmpty) Some(right) else None

  override def readUniversalQuantifier(left: String, right: String): Option[(String, String)] = ???

  override def readExistentialQuantifier(left: String, right: String): Option[(String, String)] = ???



}
