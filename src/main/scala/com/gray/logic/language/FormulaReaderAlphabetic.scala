package com.gray.logic.language

import com.gray.logic.mixin.{ControlFlow, ReadUtils}

object FormulaReaderAlphabetic extends FormulaReader with ControlFlow with ReadUtils {

  private def aInt = 'a'.toInt

  private def AInt = 'A'.toInt

  private def vInt = 'v'.toInt

  private def fInt = 'f'.toInt

  private def RInt = 'R'.toInt

  override def before(string: String): String = string.replaceAll(" +", "")

  override def readSentence(string: String): Option[Int] = if (string.length == 1) {
    val asInt = string(0).toInt
    if (AInt <= asInt && 'L'.toInt >= asInt) Some(asInt - AInt)
    else None
  } else None

  override def readEquals(string: String): Option[(String, String)] = rangeOfTermAt(string, 0) match {
    case Some(t1Range) =>
      val t1String = string.substring(0, t1Range._2)
      val remainder = string.substring(t1Range._2)
      if (remainder.startsWith("=")) {
        val remainder2 = remainder.substring(1)
        rangeOfTermAt(remainder2, 0) match {
          case Some((_, end)) if end == remainder2.length =>
            Some((t1String, remainder2))
          case _ => None
        }
      }else None
    case _ => None
  }

  override def readRelation(string: String): Option[(Int, Seq[String])] = if (string.length>1) {
    val asInt = string(0).toInt
    val indexOpt = if ('R'.toInt <= asInt && asInt <= 'Z'.toInt) {
      Some(asInt - RInt)
    } else if ('M'.toInt <= asInt && asInt < RInt) {
      Some(Math.abs(asInt - RInt) + 8)
    } else None

    indexOpt match {
      case None => None
      case Some(index) =>
        val argString = string.substring(1)
        termSequence(argString) match {
          case None => None
          case Some(argStrings) =>
            Some((index, argStrings))
        }
    }
  }else None

  def termSequence(string: String, soFar: Seq[String] = Nil): Option[Seq[String]] = if (string.length>0){
    rangeOfTermAt(string, 0) match {
      case None => None
      case Some((beginnning, end)) =>
        val newSeq = soFar :+ string.substring(beginnning, end)
        val remainder = string.substring(end)
        termSequence(remainder, newSeq)
    }
  }else Some(soFar)


  def rangeOfTermAt(string: String, at: Int = 0): Option[(Int, Int)] = {
    if (string.length == at+1 || string.charAt(at+1)!='('){
      if (readConstant(string(at).toString).isDefined) Some((at, at + 1))
      else if (readVariable(string(at).toString).isDefined) Some((at, at + 1))
      else None
    } else {
      rangeOfBrackets(string, at + 1) match {
        case None =>
          None
        case Some((begin, end)) =>
          if (readFunction(string.substring(at, end)).isDefined) {
            Some((at, end))
          }else None
      }
    }
  }

  override def readVariable(string: String): Option[Int] = if (string.length == 1) {
    val asInt = string(0).toInt
    if (vInt <= asInt && asInt <= 'z'.toInt) {
      Some(asInt - vInt)
    } else if ('m'.toInt <= asInt && asInt < vInt) {
      Some(Math.abs(asInt - vInt) + 4)
    } else None
  } else None

  override def readConstant(string: String): Option[Int] = if (string.length == 1) {
    val asInt = string(0).toInt
    if (aInt <= asInt && asInt <= 'l'.toInt) {
      Some(asInt - aInt)
    } else None
  } else None

  override def readFunction(string: String): Option[(Int, Seq[String])] = if (string.length>2){
    val asInt = string(0).toInt
    val indexOpt = if (fInt <= asInt && asInt <= 'm'.toInt) {
      Some(asInt - fInt)
    } else if (aInt <= asInt && asInt < fInt) {
      Some(Math.abs(asInt - fInt) + 6)
    } else None

    indexOpt match {
      case None => None
      case Some(index) =>
        rangeOfBrackets(string, 1) match {
          case Some(range) =>
            val argString = string.substring(2, range._2-1)
            termSequence(argString) match {
              case Some(seq) =>
                Some((index, seq))
              case None => None
            }
          case _ => None
        }
    }
  }else None

  override def readConjunction(string: String): Option[(String, String)] = continueIf(string.startsWith("(") && string.endsWith(")")) {
    val unwrapped = unwrapBrackets(string)
    for (i <- 0 until unwrapped.length if unwrapped(i) == '⋀' || unwrapped(i) == '&') {
      val f1 = unwrapped.substring(0, i)
      val f2 = unwrapped.substring(i + 1)
      if (read(f1).isDefined && read(f2).isDefined) return Some(f1, f2)
    }
    None
  }

  override def readDisjunction(string: String): Option[(String, String)] = continueIf(string.startsWith("(") && string.endsWith(")")) {
    val unwrapped = unwrapBrackets(string)
    for (i <- 0 until unwrapped.length if unwrapped(i) == '⋁' || unwrapped(i) == 'V' || unwrapped(i) == 'v') {
      val f1 = unwrapped.substring(0, i)
      val f2 = unwrapped.substring(i + 1)
      if (read(f1).isDefined && read(f2).isDefined) return Some(f1, f2)
    }
    None
  }

  override def readConditional(string: String): Option[(String, String)] = continueIf(string.startsWith("(") && string.endsWith(")")) {
    val unwrapped = unwrapBrackets(string)
    for (i <- 0 until unwrapped.length if unwrapped(i) == '⟶' || unwrapped.substring(i).startsWith("->")) {
      val f1 = unwrapped.substring(0, i)
      val f2 = unwrapped.substring(i + {
        if (string(i) == '⟶') 1 else 2
      })
      if (read(f1).isDefined && read(f2).isDefined) return Some(f1, f2)
    }
    None
  }

  override def readBiconditional(string: String): Option[(String, String)] = continueIf(string.startsWith("(") && string.endsWith(")")) {
    val unwrapped = unwrapBrackets(string)
    for (i <- 0 until unwrapped.length if unwrapped(i) == '⟷' || unwrapped.substring(i).startsWith("<->")) {
      val f1 = unwrapped.substring(0, i)
      val f2 = unwrapped.substring(i + {
        if (string(i) == '⟷') 1 else 3
      })
      if (read(f1).isDefined && read(f2).isDefined) return Some(f1, f2)
    }
    None
  }

  override def readNegation(string: String): Option[String] =
    if (string.startsWith("~") || string.startsWith("¬")) Some(string.substring(1)) else None


  override def readUniversalQuantifier(string: String): Option[(String, String)] = if (string.length > 2 && (string.startsWith("∀") || string.startsWith("A"))) {
    val stripQuantifier = string.substring(1)
    if (readVariable(stripQuantifier.substring(0, 1)).isDefined) {
      Some((stripQuantifier.substring(0, 1), stripQuantifier.substring(1)))
    } else None
  } else None

  override def readExistentialQuantifier(string: String): Option[(String, String)] = if (string.length > 2 && (string.startsWith("∃") || string.startsWith("E"))) {
    val stripQuantifier = string.substring(1)
    if (readVariable(stripQuantifier.substring(0, 1)).isDefined) {
      Some((stripQuantifier.substring(0, 1), stripQuantifier.substring(1)))
    } else None
  } else None

}