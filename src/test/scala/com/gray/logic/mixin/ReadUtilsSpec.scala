package com.gray.logic.mixin

import org.scalatest.{FlatSpec, Matchers}

class ReadUtilsSpec extends FlatSpec with ReadUtils with Matchers {

  "rangeOfBrackets" should "find the range of a bracket expression from an opening bracket" in {
    val str = "(this(that)and(the(other)))"
    rangeOfBrackets(str) shouldBe Some((0, 27))
  }

  it should "find the range of a bracket expression from an opening bracket within a string" in {
    val str = "(this(that)and(the(other)))"
    rangeOfBrackets(str, 5) shouldBe Some((5, 11))
  }

  it should "return none if the from index is not an opening bracket" in {
    val str = "this is a (string)"
    rangeOfBrackets(str, 2) shouldBe None
  }

  it should "return none if the bracket does not close" in {
    val str = "((string)"
    rangeOfBrackets(str, 0) shouldBe None
  }

}
