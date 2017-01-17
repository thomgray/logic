package com.gray.logic.language

import com.gray.logic.formula._
import com.gray.logic.mixin.ControlFlow

object LanguageAlphabetic extends Language with ControlFlow {
  //terms
  private def aInt = 'a'.toInt

  private def AInt = 'A'.toInt

  private def vInt = 'v'.toInt

  private def fInt = 'f'.toInt

  private def RInt = 'R'.toInt

  override def tokenForVariable(variable: Variable): String = if (variable.index < 5) {
    (vInt + variable.index).toChar.toString
  } else (vInt - variable.index + 4).toChar.toString

  override def tokenForConstant(constant: Constant): String = (aInt + constant.index).toChar.toString

  override def tokenForFunction(function: Function): String = (fInt + function.index).toChar.toString

  //formulas
  override def tokenForSentence(sentence: Sentence): String = (AInt + sentence.index).toChar.toString

  override def tokenForRelation(relation: Relation): String = (RInt + relation.index).toChar.toString

  override def tokenForEquals(equals: Equals): String = "="

  //conjunctives
  override def tokenForConnective(connective: Connective): String = connective match {
    case Conjunction(_, _) => "⋀"
    case Disjunction(_, _) => "⋁"
    case Conditional(_, _) => "⟶"
    case Biconditional(_, _) => "⟷"
    case Negation(_) => "¬"
  }

  override def tokenForQuantifier(quantifier: Quantifier): String = quantifier match {
    case UniversalQuantifier(_, _) => "∀"
    case ExistentialQuantifier(_, _) => "∃"
  }

  ///reading

  //formulas
  override def readSentenceToken(string: String, at: Int) =  {
    val asInt = string(0).toInt
    if (AInt <= asInt && 'L'.toInt >= asInt) Some((asInt - AInt, StringSegments(string, at)))
    else None
  }

  override def readRelationToken(string: String, at: Int) = {
    val asInt = string(at).toInt
    val indexOpt = if ('R'.toInt <= asInt && asInt <= 'Z'.toInt) {
      Some(asInt - RInt)
    } else if ('M'.toInt <= asInt && asInt < RInt) {
      Some(Math.abs(asInt - RInt) + 8)
    } else None

    indexOpt match {
      case Some(index) => Some((index, StringSegments(string, at)))
      case _ => None
    }
  }

  override def readEqualsToken(string: String, at: Int) = string(at) match {
    case '=' => Some(StringSegments(string, at))
    case _ => None
  }

  //compositors
  override def readConnectiveToken(string: String, at: Int) = string.substring(at) match {
    case str if str.startsWith("⋀") || str.startsWith("&") => Some((ConnectiveType.Conjunction, StringSegments(string, at)))
    case str if str.startsWith("⋁") || str.startsWith("V") || str.startsWith("v") => Some((ConnectiveType.Conjunction, StringSegments(string, at)))
    case str if str.startsWith("⟶") => Some((ConnectiveType.Conjunction, StringSegments(string, at)))
    case str if str.startsWith("->") => Some((ConnectiveType.Conjunction, StringSegments(string, at, 2)))
    case str if str.startsWith("⟷") => Some((ConnectiveType.Conjunction, StringSegments(string, at)))
    case str if str.startsWith("<->") => Some((ConnectiveType.Conjunction, StringSegments(string, at, 3)))
    case str if str.startsWith("¬") || str.startsWith("~") => Some((ConnectiveType.Conjunction, StringSegments(string, at)))
    case _ => None
  }

  override def readQuantifierToken(string: String, at: Int) = string(at) match {
    case '∀' | 'A' => Some((QuantifierType.UniversalQuantifier, StringSegments(string, at)))
    case '∃' | 'E' => Some((QuantifierType.ExistentialQuantifier, StringSegments(string, at)))
    case _ => None
  }

  //terms
  override def readFunctionToken(string: String, at: Int) = {
    val asInt = string(0).toInt
    val indexOpt = if (fInt <= asInt && asInt <= 'm'.toInt) {
      Some(asInt - fInt)
    } else if (aInt <= asInt && asInt < fInt) {
      Some(Math.abs(asInt - fInt) + 6)
    } else None

    indexOpt match {
      case Some(index) =>
        Some((index, StringSegments(string, at)))
      case _ => None
    }
  }

  override def readConstantToken(string: String, at: Int) = {
    val asInt = string(at).toInt
    if (aInt <= asInt && asInt <= 'l'.toInt)
      Some((asInt - aInt, StringSegments(string, at)))
    else None
  }


  override def readVariableToken(string: String, at: Int) = {
    val asInt = string(0).toInt
    val indexOpt = if (vInt <= asInt && asInt <= 'z'.toInt) {
      Some(asInt - vInt)
    } else if ('m'.toInt <= asInt && asInt < vInt) {
      Some(Math.abs(asInt - vInt) + 4)
    } else None

    indexOpt match {
      case Some(index) => Some((index, StringSegments(string, at)))
      case _ => None
    }
  }
}
