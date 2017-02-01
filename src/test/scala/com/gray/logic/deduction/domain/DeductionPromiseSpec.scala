package com.gray.logic.deduction.domain

import com.gray.logic.deduction.{DeductionNode, DeductionSequence, InferenceRule}
import com.gray.logic.deduction.inference.{Unproven, DeductionStackDud, Proven}
import com.gray.logic.formula.Sentence
import org.scalatest.{FlatSpec, FunSuite, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration._

class DeductionPromiseSpec extends FlatSpec with Matchers {

  val dudSuccess = Proven(DeductionNode(Sentence(0), InferenceRule.Premise, Nil), DeductionSequence(), DeductionStackDud)

  "future" should "complete with a success if any operation is successful" in {
    val f1 = {() => Unproven}
    val f2 = {() => Unproven}
    val f3 = {() => Unproven}
    val f4 = {() => Thread.sleep(100); dudSuccess}

    val promise = DeductionPromise(Seq(f1, f2, f3, f4))
    Await.result(promise.future, 1 second) shouldBe dudSuccess
  }

  it should "end with a failure if no operation is successful" in {
    val f1 = {() => Unproven}
    val f2 = {() => Unproven}
    val f3 = {() => Unproven}

    val promise = DeductionPromise(Seq(f1, f2, f3))
    Await.result(promise.future, 5 milliseconds) shouldBe Unproven
  }

}
