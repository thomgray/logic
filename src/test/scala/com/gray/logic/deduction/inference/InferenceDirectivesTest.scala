package com.gray.logic.deduction.inference

import com.gray.logic.deduction._
import com.gray.logic.formula.{Formula, Sentence}
import com.gray.logic.language.{FormulaReaderAlphabetic, FormulaWriterAlphabetic}
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.implicitConversions

class InferenceDirectivesTest extends FlatSpec with Matchers with InferenceDirectives {

  implicit val _ = scala.concurrent.ExecutionContext.Implicits.global

  val dudSuccess = Proven(DeductionNode(Sentence(0), InferenceRule.Premise, Nil), DeductionSequence())

//  implicit val writer = FormulaWriterAlphabetic
  implicit val reader = FormulaReaderAlphabetic

  implicit def stringToFormula(string: String): Formula = Formula.read(string).get

  "worthDoingDNI" should "run if the deduction contains the negation of the conclusion" in {
    val sequence = DeductionSequence("~A")
    val conclusion: Formula = "A"

    worthDoingDNE(conclusion, sequence)(()=> dudSuccess) shouldBe dudSuccess
  }

  it should "fail if the deduction does not contain the negation of the conclusion" in {
    val sequence = DeductionSequence("A")
    worthDoingDNE("~A", sequence)(()=> dudSuccess) shouldBe Unproven
  }

  "firstSuccess" should "return a success if one process is successful" in {
    val result = firstSuccess(Seq(
      () => Unproven,
      () => {Thread.sleep(100); dudSuccess}
    ))
    result shouldBe dudSuccess
  }

  it should "return a failure if no process is successful" in {
    val result = firstSuccess(Seq(
      () => Unproven,
      () => Unproven
    ))

    result shouldBe Unproven
  }

  it should "return a success as soon as a process is successful" in {
    val result = Future{
      firstSuccess(Seq(
        () => Unproven,
        () => Unproven,
        {() => Thread.sleep(10000); Unproven},
        {() => Thread.sleep(1000); dudSuccess}
      ))}
    Thread.sleep(2000)
    Await.result(result, 0 nanos) shouldBe dudSuccess
  }

  it should "return a failure as soon as all processes are finished unsuccessfully" in {
    val result = Future{
      firstSuccess(Seq(
        () => Unproven,
        () => Unproven,
        {() => Thread.sleep(100); Unproven},
        {() => Thread.sleep(1000); Unproven}
      ))}
    Thread.sleep(500)
    result.isCompleted shouldBe false
    Thread.sleep(600)
    Await.result(result, 0 nanos) shouldBe Unproven
  }


}
