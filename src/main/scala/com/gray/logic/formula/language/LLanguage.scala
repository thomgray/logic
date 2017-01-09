package com.gray.logic.formula.language

import com.gray.logic.formula.elements.LConnectiveType.ConnectiveType
import com.gray.logic.formula.elements.LQuantifierType.QuantifierType

trait LLanguage {

  //writing
  def tokenForFunction(index: Int): String
  def tokenForConstant(index: Int): String
  def tokenForVariable(index: Int): String

  def tokenForSentence(index: Int): String
  def tokenForRelation(index: Int): String
  def tokenForEquals(): String

  def tokenForConnective(connectiveType: ConnectiveType): String
  def tokenForQuantifier(quantifierType: QuantifierType): String

  //reading
  def variableForToken(string: String): Int
  def constantForToken(string: String): Int
  def functionForToken(string: String): Int
  
  def sentenceForToken(string: String): Int
  def relationForToken(string: String): Int
  def equalsFromToken(string: String): Boolean

  def quantifierForToken(string: String): QuantifierType
  def connectiveForToken(string: String): ConnectiveType
  
}
