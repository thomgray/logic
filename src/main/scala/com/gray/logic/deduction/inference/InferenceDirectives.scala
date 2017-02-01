package com.gray.logic.deduction.inference

import com.gray.logic.deduction.DeductionSequence
import com.gray.logic.deduction.domain.DeductionPromise
import com.gray.logic.formula.{Formula, Negation}
import com.gray.logic.tools.Logging

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Success, Try}

trait InferenceDirectives extends Logging {

  private val timeoutSeconds = Duration(10, "seconds")
  def worthDoingDNE(conclusion: Formula, sequence: DeductionSequence)(block: () => Result) = conclusion match {
    case _: Negation =>
      val negated = Negation(Negation(conclusion))
      val negations = InferenceUtils.allFormulaDecompositions(sequence).collect {
        case neg: Negation => neg
      }
      if (negations.contains(negated)) block() else Unproven
    case _ => block()
  }

  def firstSuccess(seq: Seq[(() => Result)]): Result = Try {
    Await.result(DeductionPromise(seq).future, timeoutSeconds)
  } match {
    case Success(result) => result
    case _ =>
      logger.info(s"operations timed out after ${timeoutSeconds.length} ${timeoutSeconds._2}")
      Unproven
  }


}
