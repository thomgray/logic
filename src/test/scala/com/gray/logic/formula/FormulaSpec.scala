package com.gray.logic.formula

import org.scalatest.{FlatSpec, Matchers}

class FormulaSpec extends FlatSpec with Matchers {

  "negation" should "negate a sentence" in {
    val sentence = Sentence(0)
    val negation = sentence.negation()

    negation shouldBe Negation(Sentence(0))
  }

  it should "un-negate a negation" in {
    val negation = Negation(Sentence(0))
    negation.negation() shouldBe Sentence(0)
  }

  "negationStrict" should "negate a negation" in {
    val negation = Negation(Sentence(0))
    val negNegation = Negation(negation)

    negation.negationStrict() shouldBe negNegation
  }

}
