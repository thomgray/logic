package com.gray.logic.deduction

import com.gray.logic.formula.{FormulaWriter_Alphabetic, LFormula}
import com.gray.logic.formula.elements.LConnectiveType._
import com.gray.logic.formula.elements._
import com.gray.logic.deduction.InferenceRules._

class DeductionSoft(conclusion: LFormula, premises: LFormula*) extends DeductionBase(conclusion, premises: _*) {


  def MP_Soft(conc: LFormula): Option[DeductionNode] = {
    val mps = findNodes (f => f.root match {
      case LConnective(Conditional) if f.composition.last == conc => true
      case _ => false
    })

    mps.map{ node =>
      val antecedent = node.formula.composition.head
      findNode(antecedent) match {
        case Some(antInDeduction) =>
          return Some(infer(conc, ModusPonens, Seq(node, antInDeduction)))
        case _ => false
      }
    }
    None
  }

}
