package com.gray.logic.deduction.inference

trait Decision {

  this: {
    def inferRAA_Hard(deductionRequest: DeductionRequest): Result
  } =>

  def attempt(deductionRequest: DeductionRequest): Seq[(DeductionRequest)=>Result] = {
    Seq(
      inferRAA_Hard
    )
  }

  private def shouldAttemptRAA(deductionRequest: DeductionRequest) = {

  }
}
