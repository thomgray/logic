package com.gray.logic.formula

import com.gray.logic.formula.elements._
import org.scalatest._

class LFormulaInitSpec extends FlatSpec with MustMatchers with BeforeAndAfter {

  "init" should "thow an exception if a compositor is specified without composits" in {
    assertThrows[IllegalArgumentException](new LFormula(LConnective.conjunction))
  }

  it should "throw an exception if an atomic formula is specified with composits" in {
    assertThrows[IllegalArgumentException](new LFormula(LSentence(1), Seq(LFormula(LSentence(0)))))
  }

  "conjunction" should "initialise a conjunction with atomic formulas" in {
    val formula = LFormula.conjunction(LSentence(1), LSentence(2))
    formula.isComposite mustBe true
    formula.mainConnective.get mustBe LConnective.conjunction
    formula.composition.length mustBe 2
    formula.composition(0).isAtomic mustBe true
    formula.composition(0).root mustBe LSentence(1)
    formula.composition(1).isAtomic mustBe true
    formula.composition(1).root mustBe LSentence(2)
  }

  "disjunction" should "initialise a disjunction with atomic formulas" in {
    val f = LFormula.disjunction(LSentence(0), LSentence(1))
    f.isComposite mustBe true
    f.mainConnective.get mustBe LConnective.disjunction
    f.composition.length mustBe 2
    f.composition(0).root mustBe LSentence(0)
    f.composition(1).root mustBe LSentence(1)
  }

  "conditional" should "initialise a conditional with atomic formulas" in {
    val f = LFormula.conditional(LSentence(0), LSentence(1))
    f.isComposite mustBe true
    f.mainConnective.get mustBe LConnective.conditional
    f.composition.length mustBe 2
    f.composition(0).root mustBe LSentence(0)
    f.composition(1).root mustBe LSentence(1)
  }

  "biconditional" should "initialise a biconditional with atomic formulas" in {
    val f = LFormula.biconditional(LSentence(0), LSentence(1))
    f.isComposite mustBe true
    f.mainConnective.get mustBe LConnective.biconditional
    f.composition.length mustBe 2
    f.composition(0).root mustBe LSentence(0)
    f.composition(1).root mustBe LSentence(1)
  }

  "negation" should "initialise a formaula with a negaiton" in {
    val f = LFormula.negationStrict(LSentence(1))
    f.isComposite mustBe true
    f.mainConnective.get mustBe LConnective.negation
    f.composition.length mustBe 1
    f.composition(0).root mustBe LSentence(1)
  }

  "equals" should "match atomic sentences" in {
    val f1 = LFormula(LSentence(1))
    val f2 = LFormula(LSentence(1))
    f1 mustBe f2
  }
  
}
