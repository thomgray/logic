package com.gray.logic.operation

import com.gray.logic.formula
import com.gray.logic.formula._
import com.gray.logic.language.FormulaWriterAlphabetic
import org.scalatest.{FunSuite, Matchers}

class ExtractionSpec extends FunSuite with Matchers with Extraction {

  implicit val writer = FormulaWriterAlphabetic

  test("allTerms should procude all terms for a set of formulas") {
    val constants = for (i <- 0 to 2) yield Constant(i)
    val variables = for (i <- 0 to 2) yield Variable(i)
    val f1 = formula.Function(0, Seq(constants(0), variables(0)))
    val f2 = formula.Function(1, Seq(constants(1), variables(1)))
    val f3 = formula.Function(2, Seq(constants(2), variables(2)))

    val R1 = Relation(0, Seq(constants(0), f1))
    val R2 = Relation(1, Seq(constants(2), variables(0)))
    val R3 = Relation(2, Seq(constants(1), f2))

    val domain = Set[Formula](R1, R2, R3)

    val terms = allTerms(domain)

    terms shouldEqual Set(constants(0), variables(0), constants(2), constants(1), variables(1), f1, f2)
  }

  test("allFormulaDecompositions should decompose all formulas") {
    val s0 = Sentence(0)
    val s1 = Sentence(1)
    val s2 = Sentence(2)
    val s3 = Sentence(3)
    val s4 = Sentence(4)

    val f1 = Conditional(s0, s1)
    val f2 = Negation(Biconditional(s2, Disjunction(s3, s4)))

    val f = allFormulaDecompositions(Seq(f1, f2))

    f should contain(f1)
    f should contain(f2)
    f should contain(s0)
    f should contain(s1)
    f should contain(s2)
    f should contain(s3)
    f should contain(s4)
    f should contain(Disjunction(s3, s4))
    f should contain(Biconditional(s2, Disjunction(s3, s4)))
    f should contain(Conditional(s2, Disjunction(s3, s4)))
    f should contain(Conditional(Disjunction(s3, s4), s2))

    f.length shouldBe 11
  }

}