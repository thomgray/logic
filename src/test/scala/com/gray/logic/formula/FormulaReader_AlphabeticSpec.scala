package com.gray.logic.formula

import com.gray.logic.formula.elements._
import org.scalatest.{FlatSpec, Matchers}

class FormulaReader_AlphabeticSpec extends FlatSpec with Matchers {

  implicit val reader = FormulaReader_Alphabetic
  implicit val writer = FormulaWriter_Alphabetic

  "read" should "read a sentence" in {
    reader.read("A") shouldBe LFormula(LSentence(0))
  }

  it should "read a relation" in {
    LFormula.read("Raa") shouldBe LFormula(LRelation(0, Seq(LConstant(0), LConstant(0))))
  }

  it should "read a relation with mixed terms" in {
    val expected = LFormula(LRelation(2, Seq(LConstant(0), LVariable(0), LFunction(0, Seq(LConstant(1))))))
    val actual = reader.read("Tavf(b)")
    expected shouldBe actual
  }

  "readConnective" should "identify a main connective for a simple composite formula" in {
    val str = "(A∧B)"
    val res = reader.readConnective(str)
    val s1 = LFormula(LSentence(0))
    val s2 = LFormula(LSentence(1))
    res match {
      case Some((conType, Seq(c1,c2))) =>
        conType shouldBe LConnectiveType.Conjunction
        c1 shouldBe "A"
        c2 shouldBe "B"
      case _ => fail
    }
  }

  it should "identify a main connective for a bracketed composite formula" in {
    val str = "((this is a thing)∧(that is a thing))"
    val res = reader.readConnective(str)
    val s1 = LFormula(LSentence(0))
    val s2 = LFormula(LSentence(1))
    res match {
      case Some((conType, Seq(c1,c2))) =>
        conType shouldBe LConnectiveType.Conjunction
        c1 shouldBe "(this is a thing)"
        c2 shouldBe "(that is a thing)"
      case _ => fail
    }
  }

  "readFunction()" should "read a function with one argument" in {
    reader.readFunction("f(a)") shouldBe Some(0, Seq("a"))
  }

  "readVariable()" should "read variable indexes correctly" in {
    reader.readVariable("v") shouldBe Some(0)
    reader.readVariable("w") shouldBe Some(1)
    reader.readVariable("z") shouldBe Some(4)
    reader.readVariable("u") shouldBe Some(5)
    reader.readVariable("t") shouldBe Some(6)
    reader.readVariable("r") shouldBe Some(8)
    reader.readVariable("m") shouldBe Some(13)
  }

  it should "not read constant letters" in {
    reader.readVariable("a") shouldBe None
    reader.readVariable("l") shouldBe None
  }


  "rangeOfTerm()" should "find the range of a constant term" in {
    reader.rangeOfTerm("a") shouldBe Some((0,1))
  }


  it should "find the range of a function" in {
    reader.rangeOfTerm("f(abc)=x") shouldBe Some((0,6))
  }

  "rangeOfRelation" should "find the range of a relation with mixed terms" in {
    reader.rangeOfRelation("Ravf(c)") shouldBe Some(0, 7)
  }




}
