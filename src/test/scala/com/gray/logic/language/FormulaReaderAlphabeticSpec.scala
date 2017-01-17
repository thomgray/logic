package com.gray.logic.language

import com.gray.logic.formula._
import org.scalatest.{FlatSpec, Matchers}

class FormulaReaderAlphabeticSpec extends FlatSpec with Matchers {

  val reader = FormulaReaderAlphabetic

  "read" should "read a sentence" in {
    reader.read("A") shouldBe Some(Sentence(0))
  }

  it should "read a conjunction" in {
    reader.read("(A&B)") shouldBe Some(Conjunction(Sentence(0), Sentence(1)))
  }

  it should "read a disjunction" in {
    reader.read("(AVB)") shouldBe Some(Disjunction(Sentence(0), Sentence(1)))
  }

  it should "read a conditional" in {
    reader.read("(A->B)") shouldBe Some(Conditional(Sentence(0), Sentence(1)))
  }

  it should "read a biconditional" in {
    reader.read("(A<->B)") shouldBe Some(Biconditional(Sentence(0), Sentence(1)))
  }

  it should "read a relation" in {
    reader.read("Ra") shouldBe Some(Relation(0, Seq(Constant(0))))
  }

  it should "read a conjunction with relations" in {
    reader.read("(Ra&Sbc)") shouldBe Some(Conjunction(Relation(0, Seq(Constant(0))), Relation(1, Seq(Constant(1), Constant(2)))))
  }

  it should "read a relation with complex functions" in {
    reader.read("Raf(bg(c))") shouldBe Some(Relation(0, Seq(Constant(0), Function(0, Seq(Constant(1), Function(1, Seq(Constant(2))))))))
  }

  "readConstant" should "read a constant" in {
    reader.readConstant("a") shouldBe Some(0)
  }

  "readVariable" should "read a variable" in {
    reader.readVariable("v") shouldBe Some(0)
  }

  "termSequence" should "find a sequence of constants" in {
    reader.termSequence("abc") shouldBe Some(Seq("a", "b", "c"))
  }

  "rangeOfTerm" should "find the range of a constant" in {
    reader.rangeOfTermAt("a", 0) shouldBe Some((0, 1))
  }

  it should "find the range of a variable" in {
    reader.rangeOfTermAt("v") shouldBe Some(0,1)
  }

  it should "find the range of a function" in {
    reader.rangeOfTermAt("f(a)") shouldBe Some(0, 4)
  }


}
