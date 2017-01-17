package com.gray.logic.language

import com.gray.logic.formula._
import com.gray.logic.mixin.ControlFlow

class FormulaParser(language: Language, syntax: Syntax) extends ControlFlow {

  def read(string: String) = readRecursive(string)
  def write(formula: Formula) = ???

  private def readRecursive(string: String): Option[Formula] = continueIf[Formula](string.length>0) {
    for (i <- 0 until string.length){
      readSentence(string, i) match {
        case Some(sentence) => return Some(sentence)
        case _ =>
      }



    }

    None
  }

  private def readSentence(string: String, at: Int) = {
    language.readSentenceToken(string, at) match {
      case Some((index, StringSegments(left, right))) =>
        syntax.readSentence(left, right) match {
          case true => Some(Sentence(index))
          case false => None
        }
      case _ => None
    }
  }


  private def readFunction(string: String, at: Int) = {
    language.readFunctionToken(string, at) match {
      case Some((index, StringSegments(left, right))) =>
        syntax.readFunction(left, right) match {
          case Some(argStrings) =>
            val args = argStrings.flatMap(readTerm(_, 0))
            if (args.length==argStrings.length) Some(Function(index, args))
            else None
          case None => None
        }
      case _ => None
    }
  }

  private def readTerm(string: String, at: Int): Option[Term] = ???

}
