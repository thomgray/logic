package cucumber.steps

import cucumber.api.scala.{EN, ScalaDsl}
import org.scalatest.Matchers

class DeductionBaseSteps extends ScalaDsl with EN with Matchers {

  val timeoutInSeconds = 12
  val holder = DeductionHolder

  Before { f =>
    holder.refresh()
  }


}