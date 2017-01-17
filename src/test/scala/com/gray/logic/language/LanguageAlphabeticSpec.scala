package com.gray.logic.language

import com.gray.logic.formula.Variable
import org.scalatest.{FlatSpec, Matchers}

class LanguageAlphabeticSpec extends FlatSpec with Matchers {
  val language = LanguageAlphabetic

//  "tokenForVariable" should "be v for index 0" in {
//    language.tokenForVariable(Variable(0)) shouldBe "v"
//  }
//
//  it should "be u for index 5" in {
//    language.tokenForVariable(Variable(5)) shouldBe "u"
//  }
//
//  "readRelationToken" should "count from R to Z then Q to M" in {
//    language.readRelationToken("R", 0) shouldBe Some(0)
//    language.readRelationToken("V", 0) shouldBe Some(4)
//    language.readRelationToken("Z") shouldBe Some(8)
//    language.readRelationToken("Q") shouldBe Some(9)
//    language.readRelationToken("M") shouldBe Some(13)
//    language.readRelationToken("L") shouldBe None
//  }
//
//  "readVariableToken" should "count from v to z then u to m" in {
//    language.readVariableToken("v") shouldBe Some(0)
//    language.readVariableToken("z") shouldBe Some(4)
//    language.readVariableToken("u") shouldBe Some(5)
//    language.readVariableToken("m") shouldBe Some(13)
//    language.readVariableToken("l") shouldBe None
//  }
//
//  "readConstant" should "count from a to l" in {
//    language.readConstantToken("a") shouldBe Some(0)
//    language.readConstantToken("l") shouldBe Some(11)
//    language.readConstantToken("m") shouldBe None
//  }

}
