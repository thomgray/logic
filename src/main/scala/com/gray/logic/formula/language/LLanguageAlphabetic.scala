package com.gray.logic.formula.language
import com.gray.logic.formula.elements.LConnectiveType._
import com.gray.logic.formula.elements.LQuantifierType._

object LLanguageAlphabetic extends LLanguage {

  override def tokenForFunction(index: Int): String = ('f'.toInt+index).toChar.toString

  override def tokenForConstant(index: Int): String = ('a'.toInt+index).toChar.toString

  override def tokenForVariable(index: Int): String =
    if (index<5) ('v'.toInt+index).toChar.toString else ('v'.toInt+4-index).toChar.toString

  override def tokenForSentence(index: Int): String = ('A'.toInt+index).toChar.toString

  override def tokenForRelation(index: Int): String =
    if (index < 9) ('R'.toInt+index).toChar.toString else ('R'.toInt+8-index).toChar.toString

  override def tokenForEquals(): String = "="

  override def tokenForConnective(connectiveType: ConnectiveType): String = connectiveType match {
    case Conjunction => "∧"
    case Disjunction => "∨"
    case Conditional => "→"
    case Biconditional => "⟷"
    case Negation => "¬"
  }

  override def tokenForQuantifier(quantifierType: QuantifierType): String = quantifierType match {
    case Universal => "∀"
    case Existential => "∃"
  }

  //reading
  override def variableForToken(string: String): Int = {
    val char = string(0).toInt
    val mInt = 'm'.toInt
    val vInt = 'v'.toInt
    if (char >= vInt) char - vInt
    else Math.abs(char - vInt) + 4
  }

  override def constantForToken(string: String): Int = string(0).toInt - 'a'.toInt

  override def functionForToken(string: String): Int = ???

  override def sentenceForToken(string: String): Int = string(0).toInt - 'A'.toInt

  override def relationForToken(string: String): Int = ???

  override def equalsFromToken(string: String): Boolean = ???

  override def quantifierForToken(string: String): QuantifierType = string match {
    case "∀" => Universal
    case "∃" => Existential
  }

  override def connectiveForToken(string: String): ConnectiveType = string match {
    case "&" | "∧" => Conjunction
    case "->" | "→" => Conditional
    case "V" | "∨" => Disjunction
    case "<->" |  "⟷" => Biconditional
    case "~" | "¬" => Negation
  }
}
