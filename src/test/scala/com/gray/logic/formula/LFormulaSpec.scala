package com.gray.logic.formula

import com.gray.logic.formula.elements._
import org.scalatest.{FlatSpec, Matchers}

class LFormulaSpec extends FlatSpec with Matchers{

  "equals()" should "match atomic relations" in {
    val f1 = LFormula(LRelation(4, Seq(LConstant(0), LConstant(10))))
    val f2 = LFormula(LRelation(4, Seq(LConstant(0), LConstant(10))))
    f1 shouldBe f2
  }

  it should "match atomic equals" in {
    val f1 = LFormula(LEquals(LConstant(1), LVariable(2)))
    val f2 = LFormula(LEquals(LConstant(1), LVariable(2)))
    f1 shouldBe f2
  }

  it should "match composite sentential formulas" in {
    val f1 = LFormula.conditional(LSentence(0), LSentence(1))
    val f2 = LFormula.conditional(LSentence(0), LSentence(1))

    f1 shouldEqual f2
  }

  it should "match composite predicate formulas" in {
    val f1 = LFormula.disjunction(LEquals(LVariable(0), LVariable(0)), LRelation(0, Seq(LConstant(0))))
    val f2 = LFormula.disjunction(LEquals(LVariable(0), LVariable(0)), LRelation(0, Seq(LConstant(0))))

    f1 shouldBe f2
  }

  it should "match a relation" in {
    val f1 = LFormula(LRelation(0, Seq(LConstant(0), LConstant(0))))
    val f2 = LFormula(LRelation(0, Seq(LConstant(0), LConstant(0))))
    f1 shouldBe f2
  }

}
