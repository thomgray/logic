package com.gray.logic.formula

import com.gray.logic.formula.elements.{LSentence, LVariable}
import org.scalatest.{FlatSpec, Matchers}

class FormulaWriter_AlphabeticSpec extends FlatSpec with Matchers {

  implicit val writer = FormulaWriter_Alphabetic

  "writeVariable" should "write variables v-z, then u-a" in {
    writer.writeVariable(0) shouldBe "v"
    writer.writeVariable(3) shouldBe "y"
    writer.writeVariable(4) shouldBe "z"
    writer.writeVariable(5) shouldBe "u"
    writer.writeVariable(10) shouldBe "p"
    writer.writeVariable(15) shouldBe "k"
  }

  "writeFunction" should "write with with format f(a,b)" in {
    writer.writeFunction(0, Seq("a", "b")) should be ("f(a,b)")
    writer.writeFunction(0, Seq()) should be ("f()")
  }

  "writeConstant" should "write constants a-z" in {
    writer.writeConstant(0) shouldBe "a"
    writer.writeConstant(2) shouldBe "c"
    writer.writeConstant(10) shouldBe "k"
    writer.writeConstant(12) shouldBe "m"
  }
  
  "writeFormula" should "write ~(A&B)" in {
    val formula = LFormula.conjunction(LSentence(0), LSentence(1))
    val formula2 = formula.negationStrict()
    formula2.write shouldBe "¬(A∧B)"
  }

  it should "write ~a=c" in {
  }
}
