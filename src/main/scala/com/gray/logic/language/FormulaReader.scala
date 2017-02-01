package com.gray.logic.language

import com.gray.logic.formula._
import scala.util.{Failure, Success, Try}

trait FormulaReader {

  def read(string: String) = Try {
    readRecursive(before(string))
  } match {
    case Success(result) => result
    case Failure(_) => None
  }

  def apply(string: String): Option[Formula] = read(string)
  def unapply(string: String): Option[Formula] = read(string)

  def before(string: String): String = string

  private def readRecursive(string: String): Option[Formula] = if (string.nonEmpty) {
    readSentence(string) match {
      case Some(i) => return Some(Sentence(i))
      case _ =>
    }
    readEquals(string) match {
      case Some((l,r)) =>
        (readTerm(l), readTerm(r)) match {
          case (Some(left), Some(right)) =>
            return Some(Equals(left, right))
          case _ => return None
        }
      case _ =>
    }
    readRelation(string) match {
      case Some((i, argStrings)) =>
        val args = argStrings flatMap readTerm
        if (args.length==argStrings.length) {
          return Some(Relation(i, args))
        }else return None
      case _ =>
    }
    readConnective(string) match {
      case Some(f) => return Some(f)
      case _ =>
    }
    readQuantifier(string) match {
      case Some(f) => return Some(f)
      case _ =>
    }
    None
  }else None

  def readTerm(string: String): Option[Term] = if (string.nonEmpty) {
    readConstant(string) match {
      case Some(i) => return Some(Constant(i))
      case _ =>
    }
    readVariable(string) match {
      case Some(i) => return Some(Variable(i))
      case _ =>
    }
    readFunction(string) match {
      case Some((i, argStrings)) =>
        val args = argStrings flatMap readTerm
        if (args.length == argStrings.length) {
          return Some(Function(i, args))
        }else None
      case _ =>
    }
    None
  }else None

  def readConnective(string: String): Option[Connective] = {
    def readBinaryConnective(connectiveType: ConnectiveType.Value) = {
      val function = connectiveType match {
        case ConnectiveType.Conjunction => readConjunction _
        case ConnectiveType.Disjunction => readDisjunction _
        case ConnectiveType.Conditional => readConditional _
        case ConnectiveType.Biconditional => readBiconditional _
      }
      function(string) match {
        case Some((left,right)) =>
          (readRecursive(left), readRecursive(right)) match {
            case (Some(leftFormula), Some(rightFormula)) =>
              connectiveType match {
                case ConnectiveType.Conjunction => Some(Conjunction(leftFormula, rightFormula))
                case ConnectiveType.Disjunction => Some(Disjunction(leftFormula, rightFormula))
                case ConnectiveType.Conditional => Some(Conditional(leftFormula, rightFormula))
                case ConnectiveType.Biconditional => Some(Biconditional(leftFormula, rightFormula))
              }
            case _ =>  None
          }
        case _ => None
      }
    }

    Seq(ConnectiveType.Conjunction, ConnectiveType.Disjunction, ConnectiveType.Conditional, ConnectiveType.Biconditional) foreach { connectiveType =>
      readBinaryConnective(connectiveType) match {
        case Some(f) => return Some(f)
        case None =>
      }
    }

    readNegation(string) match {
      case Some(formulaString) =>
        readRecursive(formulaString) match {
          case Some(formula) => return Some(Negation(formula))
          case _ => return None
        }
      case _ =>
    }
    None
  }

  def readQuantifier(string: String): Option[Quantifier] = {
    readUniversalQuantifier(string) match {
      case Some((variableString,formulaString)) =>
        (readVariable(variableString), readRecursive(formulaString)) match {
          case (Some(varInt), Some(formula)) =>
            return Some(UniversalQuantifier(Variable(varInt), formula))
          case _ => return None
        }
      case _ =>
    }
    readExistentialQuantifier(string) match {
      case Some((variableString,formulaString)) =>
        (readVariable(variableString), readRecursive(formulaString)) match {
          case (Some(varInt), Some(formula)) =>
            return Some(ExistentialQuantifier(Variable(varInt), formula))
          case _ => return None
        }
      case _ =>
    }
    None
  }


  def readSentence(string: String): Option[Int]
  def readEquals(string: String): Option[(String, String)]
  def readRelation(string: String): Option[(Int, Seq[String])]

  def readVariable(string: String): Option[Int]
  def readConstant(string: String): Option[Int]
  def readFunction(string: String): Option[(Int, Seq[String])]

  def readConjunction(string: String): Option[(String, String)]
  def readDisjunction(string: String): Option[(String, String)]
  def readConditional(string: String): Option[(String, String)]
  def readBiconditional(string: String): Option[(String, String)]
  def readNegation(string: String): Option[String]

  def readUniversalQuantifier(string: String): Option[(String, String)]
  def readExistentialQuantifier(string: String): Option[(String, String)]

}
