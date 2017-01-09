package com.gray.logic.formula

import com.gray.logic.formula.elements.{LConnective, LConnectiveType, LSentence}
import com.gray.logic.formula.elements.LConnectiveType._
import com.gray.logic.formula.elements.LQuantifierType._
import com.gray.logic.mixin.ReadUtils

object FormulaReader_Alphabetic extends FormulaReader with ReadUtils {

  private val negationSymbols = Seq("¬", "~")
  private val conjunctionSymbols = Seq("&", "∧")
  private val disjunctionSymbols = Seq("v", "V", "∨")
  private val conditionalSymbols = Seq("→", "->")
  private val biconditionalSymbols = Seq("⟷", "<->")

  override def before(string: String): String = string.replaceAll(" +", "")

  //compositors
  override def readConnective(string: String): Option[(ConnectiveType, Seq[String])] = {
    if (negationSymbols.exists(string.startsWith)) Some(Negation, Seq(string.substring(1)))
    else if (string.startsWith("(")) {
      val unwrapped = unwrapBrackets(string)

      val s1Range = rangeOfFormula(unwrapped)

      s1Range match {
        case Some((from, to)) =>
          val segment = unwrapped.substring(to)
          var symbolOpt: Option[String] = None
          var connectiveTypeOpt: Option[ConnectiveType] = None

          for (list <- Seq(conjunctionSymbols, conditionalSymbols) if symbolOpt.isEmpty) {
            list.find(segment.startsWith) match {
              case Some(symbol) =>
                symbolOpt = Some(symbol)
                connectiveTypeOpt = list match {
                  case `conditionalSymbols` => Some(Conditional)
                  case `conjunctionSymbols` => Some(Conjunction)
                  case `disjunctionSymbols` => Some(Disjunction)
                  case `biconditionalSymbols` => Some(Biconditional)
                }
              case _ =>
            }
          }

          (symbolOpt, connectiveTypeOpt) match {
            case (Some(symbol), Some(connectiveType)) =>
              val s1 = unwrapped.substring(from, to)
              val s2 = unwrapped.substring(to + symbol.length)
              Some((connectiveType, Seq(s1, s2)))
            case _ => None
          }
        case _ => None
      }
    } else None
  }


  override def readQuantifier(string: String): Option[(QuantifierType, Int, String)] = None

  //atomic formulas
  override def readSentence(string: String): Option[Int] = if (string.length == 1) {
    string.charAt(0).toInt match {
      case c if c >= 'A'.toInt && c <= 'Z'.toInt =>
        Some(c - 'A'.toInt)
      case _ => None
    }
  } else None

  override def readEquals(string: String): Option[(String, String)] = rangeOfEquals(string) match {
    case Some(range) =>
      val t1Range = rangeOfTerm(string).get
      val t2Range = rangeOfTerm(string, t1Range._2 + 1).get
      val t1 = string.substring(t1Range._1, t1Range._2)
      val t2 = string.substring(t2Range._1, t2Range._2)
      Some((t1, t2))
    case _ => None
  }

  override def readRelation(string: String): Option[(Int, Seq[String])] = rangeOfRelation(string) match {
    case Some(range) if range._2 == string.length =>
      val index = string(0).toInt - 'R'.toInt
      val args = string.substring(1)
      Some(index, getTermList(args))
    case _ => None
  }

  //terms
  override def readFunction(string: String): Option[(Int, Seq[String])] = rangeOfFunction(string) match {
    case Some(range) if range._2 == string.length =>
      val index = string(0).toInt - 'f'.toInt
      val args = string.substring(2, string.length - 1)
      Some(index, getTermList(args))
    case _ => None
  }

  private def getTermList(string: String, listSoFar: Seq[String] = Nil): Seq[String] = rangeOfTerm(string) match {
    case Some(range) =>
      val split = string.splitAt(range._2)
      val augmentedList = listSoFar :+ split._1
      getTermList(split._2, augmentedList)
    case _ => listSoFar
  }

  override def readConstant(string: String): Option[Int] = if (string.length == 1) {
    val char = string(0).toInt
    val aInt = 'a'.toInt
    if (char >= aInt && char <= 'l'.toInt) Some(char - aInt)
    else None
  } else None

  override def readVariable(string: String): Option[Int] = if (string.length == 1) {
    val char = string(0).toInt
    val mInt = 'm'.toInt
    val vInt = 'v'.toInt

    if (char >= mInt && char <= 'z'.toInt) {
      val index = if (char >= vInt) char - vInt
      else Math.abs(char - vInt) + 4
      Some(index)
    }
    else None
  } else None


  // ranges

  private[formula] def rangeOfSentence(string: String, from: Int = 0) = {
    val asInt = string.charAt(from).toInt
    if ('A'.toInt <= asInt && asInt <= 'Z'.toInt) Some((from, from + 1))
    else None
  }


  private[formula] def rangeOfVariable(string: String, from: Int = 0): Option[(Int, Int)] = {
    val cInt = string.charAt(from).toInt
    if (cInt >= 'm'.toInt && cInt <= 'z'.toInt) {
      Some((from, from + 1))
    } else None
  }

  private[formula] def rangeOfConstant(string: String, from: Int = 0): Option[(Int, Int)] = {
    val cInt = string.charAt(from).toInt
    if (cInt >= 'a'.toInt && cInt <= 'l'.toInt) {
      if (from + 1 < string.length && string.charAt(from + 1) == '(') None
      else Some((from, from + 1))
    } else None
  }

  private[formula] def rangeOfFunction(string: String, from: Int = 0): Option[(Int, Int)] = {
    val cInt = string.charAt(from).toInt
    val isFunctionLetter = 'a'.toInt <= cInt && cInt <= 'z'.toInt
    if (isFunctionLetter) {
      rangeOfBrackets(string, from + 1) match {
        case Some((otherFrom, to)) =>
          Some((from, to))
        case None => None
      }
    } else None
  }


  private[formula] def rangeOfTerm(string: String, from: Int = 0): Option[(Int, Int)] = {
    if (from >= string.length) None
    else {
      rangeOfConstant(string, from) match {
        case Some(range) => Some(range)
        case _ =>
          rangeOfVariable(string, from) match {
            case Some(range) => Some(range)
            case _ => rangeOfFunction(string, from)
          }
      }
    }
  }

  private[formula] def rangeOfFormula(string: String, from: Int = 0) = {
    rangeOfBrackets(string, from) match {
      case Some(range) => Some(range)
      case None =>
        rangeOfSentence(string, from) match {
          case Some(range) => Some(range)
          case _ => rangeOfRelation(string, from) match {
            case Some(range) => Some(range)
            case _ => rangeOfEquals(string, from)
          }
        }
    }
  }

  private[formula] def rangeOfRelation(string: String, from: Int = 0): Option[(Int, Int)] = string charAt from match {
    case c if c.toInt <= 'Z'.toInt && c.toInt >= 'A'.toInt =>
      val `from+1` = from + 1
      def endOfArgs(at: Int): Int = rangeOfTerm(string, at) match {
        case Some((b, e)) => endOfArgs(e)
        case None => at
      }
      endOfArgs(`from+1`) match {
        case `from+1` => None
        case other => Some(from, other)
      }
    case _ => None
  }

  private[formula] def rangeOfEquals(string: String, from: Int = 0): Option[(Int, Int)] = rangeOfTerm(string, from) match {
    case Some(t1Range) =>
      if (string.charAt(t1Range._2) == '=') {
        rangeOfTerm(string, t1Range._2 + 1) match {
          case Some(t2Range) =>
            Some(t1Range._1, t2Range._2)
          case None => None
        }
      } else None
    case None => None
  }

}
