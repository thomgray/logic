package com.gray.logic.formula

import org.scalatest.FunSuite

class LogicDSLTest extends FunSuite with LogicDSL {

  val foo = something
  val bar = a thing
  val thom = some person
  val red = a property
  val x = any thing
  val `thom is red` = thom is red
  val `foo is red` = foo is red

  test("something") (assertResult(foo)(Constant(0)))
  test("some person") (assertResult(thom)(Constant(2)))
  test("a property")(assertResult(red)(RelationScheme(0)))
  test("any thing")(assertResult(x)(Variable(0)))
  test("thom is red") (assertResult(Relation(0, List(Constant(2))))(`thom is red`))
  test("foo is red") (assertResult(Relation(0, List(Constant(0))))(`foo is red`))
  test("foo is red and thom is red") { assertResult(`foo is red` and `thom is red`)(Conjunction(`foo is red`, `thom is red`)) }
  test("foo is red or thom is red") {assertResult((foo is red) or (thom is red))(Disjunction(`foo is red`, `thom is red`))}
  test("if thom is red then foo is red") {assertResult(If (`thom is red`) Then `foo is red`)(Conditional(`thom is red`, `foo is red`))}
  test("thom is red -> foo is red") {assertResult(`thom is red` -> `foo is red`)(Conditional(`thom is red`, `foo is red`))}

  test("implicit string to constant conversion") {
    val james: Constant = "james"
    assertResult(Constant(3))(james)
    val mike: Constant = "james"
    assertResult(Constant(3))(mike)
  }

  test("implicit string conversion with builder"){
    val sentence = "james" is red
    assertResult(Relation(0, Seq(Constant(3))))(sentence)
  }

  test("named sentence") {
    val james: Sentence = "james"
    val james2: Sentence = "james"
    assertResult(james)(james2)
  }

  test("implicit term and relation string builder") {
    val sentence: Relation = "james" is "angry"
    assertResult(Relation(1, Seq(Constant(3))))(sentence)
  }

  test("implicit string to formula conversion") {
    val formula = If("this") Then "that"
    assertResult(Conditional("this", "that"))(formula)
  }

  test("ifAndOnlyIf"){
    val formula = "this" ifAndOnlyIf "that"
  }
}
