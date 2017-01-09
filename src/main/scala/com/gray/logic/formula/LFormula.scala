package com.gray.logic.formula


import com.gray.logic.formula.elements._


class LFormula(rootElement: LElement with LRootElement, composits: Seq[LFormula] = Seq.empty) {

  private val _root: Either[LAtomicFormula, LCompositor] = rootElement match {
    case af: LAtomicFormula if composits.isEmpty => Left(af)
    case comp: LCompositor if composits.nonEmpty => Right(comp)
    case _: LAtomicFormula => throw new IllegalArgumentException("Atomic formulas cannot have composits!")
    case _: LCompositor => throw new IllegalArgumentException("Composite formulas must have composits!")
  }

  def mainCompositor:Option[LCompositor] = _root match {
    case Right(comp) => Some(comp)
    case _ => None
  }

  def rootFormula:Option[LAtomicFormula] = _root match {
    case Left(af) => Some(af)
    case _ => None
  }

  protected[logic] val root = _root match {
    case Left(af) => af
    case Right(comp) => comp
  }

  protected[logic] val composition: Seq[LFormula] = composits

  def isAtomic = root.isInstanceOf[LAtomicFormula]

  def isComposite = root.isInstanceOf[LCompositor]

  def mainConnective = root match {
    case comp: LCompositor => Some(comp)
    case _ => None
  }

  def conjunction(formula1: LAtomicFormula, keepLeft: Boolean = true) =
    new LFormula(LConnective.conjunction, keepLeft match {
      case true => Seq(this, new LFormula(formula1))
      case false => Seq(new LFormula(formula1), this)
    })

  def negation() = new LFormula(LConnective.negation, Seq(this))

  def negationStrict() = rootElement match {
    case LConnective(LConnectiveType.Negation) => composition(0)
    case _ => new LFormula(LConnective.negation, Seq(this))
  }

  def disjunction(formula1: LAtomicFormula, keepLeft: Boolean = true) =
    new LFormula(LConnective.disjunction, keepLeft match {
      case true => Seq(this, new LFormula(formula1))
      case false => Seq(new LFormula(formula1), this)
    })

  def conditional(formula1: LAtomicFormula, keepLeft: Boolean = true) =
    new LFormula(LConnective.conditional, keepLeft match {
      case true => Seq(this, new LFormula(formula1))
      case false => Seq(new LFormula(formula1), this)
    })

  def biconditional(formula1: LAtomicFormula, keepLeft: Boolean = true) =
    new LFormula(LConnective.biconditional, keepLeft match {
      case true => Seq(this, new LFormula(formula1))
      case false => Seq(new LFormula(formula1), this)
    })

  def write(implicit formulaWriter: FormulaWriter) = formulaWriter.write(this)

  override def equals(obj: scala.Any): Boolean = obj match {
    case formula: LFormula =>
      formula.root == root match {
        case false => false
        case true =>
          if (formula.composition.length != composition.length) false
          else {
            for (i <- composition.indices) {
              if (composition(i) != formula.composition(i)) return false
            }
            true
          }
      }
    case _ => false
  }
}

object LFormula {
  def apply(atomicFormula: LAtomicFormula): LFormula = new LFormula(atomicFormula)
  def read(string: String)(implicit formulaReader: FormulaReader) = formulaReader.read(string)

  def conjunction(formula1: LAtomicFormula, formula2: LAtomicFormula) = LFormula(formula1).conjunction(formula2)
  def conditional(formula1: LAtomicFormula, formula2: LAtomicFormula) = LFormula(formula1).conditional(formula2)
  def disjunction(formula1: LAtomicFormula, formula2: LAtomicFormula) = LFormula(formula1).disjunction(formula2)
  def biconditional(formula1: LAtomicFormula, formula2: LAtomicFormula) = LFormula(formula1).biconditional(formula2)
  def negation(formula: LAtomicFormula) = LFormula(formula).negation()
  def negationStrict(formula: LAtomicFormula) = LFormula(formula).negationStrict()


  def conjunction(formula1: LFormula, formula2: LFormula) = new LFormula(LConnective.conjunction, Seq(formula1, formula2))
  def disjunction(formula1: LFormula, formula2: LFormula) = new LFormula(LConnective.disjunction, Seq(formula1, formula2))
  def conditional(formula1: LFormula, formula2: LFormula) = new LFormula(LConnective.conditional, Seq(formula1, formula2))
  def biconditional(formula1: LFormula, formula2: LFormula) = new LFormula(LConnective.biconditional, Seq(formula1, formula2))
}
