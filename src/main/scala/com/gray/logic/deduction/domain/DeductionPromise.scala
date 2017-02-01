package com.gray.logic.deduction.domain

import java.util.concurrent.Executors

import com.gray.logic.deduction.inference.{Proven, Result, Unproven}
import com.gray.logic.tools.Logging

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}

class DeductionPromise(operations: Seq[()=> Result]) extends Logging {
//  import scala.concurrent.ExecutionContext.Implicits.global

  implicit private val ec = new ExecutionContext {
    val threadPool = Executors.newFixedThreadPool(1000)
    override def reportFailure(cause: Throwable): Unit = ()
    override def execute(runnable: Runnable): Unit = threadPool.submit(runnable)
  }

  private val pending = mutable.Map(operations.map((_, true)):_*)

  private val promise = Promise[Result]()

  operations map { f =>
    Future{
      f() match {
        case success: Proven =>
          logger.info("completing promise with a success!")
          promise.completeWith(Future.successful(success))
        case Unproven => notifyFail(f)
      }
    }
  }

  private def notifyFail(f: ()=> Result) = {
    pending(f) = false
    if (!pending.exists(_._2)) {
      logger.info("giving up promise as all operations were unsuccessful")
      promise.completeWith(Future.successful(Unproven))
    }
  }

  def future = promise.future

}

object DeductionPromise {
  def apply(operations: Seq[(() => Result)]): DeductionPromise= new DeductionPromise(operations)
}
